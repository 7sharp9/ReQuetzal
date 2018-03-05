namespace Compiler


[<AutoOpen>]
module Monoid =


    type IMonoid<'M> =
        abstract member Zero : (unit -> 'M)
        abstract member Combine : ('M * 'M -> 'M)

    let list =
        { new IMonoid<list<'T>> with 
            member __.Zero = fun () -> []
            member __.Combine = fun (l1,l2) -> l1 @ l2
        } 
    let array =
        { new IMonoid<'T []> with 
            member __.Zero = fun () -> [||]
            member __.Combine = fun (a1,a2) -> Array.append a1 a2
        } 

    let unit =
        { new IMonoid<_> with 
            member __.Zero = fun () -> ()
            member __.Combine = fun (_,_) -> ()
        } 

[<AutoOpen>]
module RWSResult =

    /// might want to change that as in the case of the compiler we do not want to 
    /// fail directly, but propagate a list of errors with priority
    /// TODO : To be defined and thought of carefully as this could be our error handling/recovering mechanism
    /// with the user of "compensators" which compensates the user code when possible
    [<NoComparison>]
    [<NoEquality>]
    type Result<'Read, 'Write, 'State , 'T, 'F> = 
        | Success of 'Read * 'Write * 'State * 'T
        | Failure of 'F 

    [<NoComparison>]
    [<NoEquality>]
    type RWSRInternal< 'Read, 'Write, 'State, 'T, 'F> = 
        RWSRInternal of ('State * 'Read -> Result<'Read , 'Write, 'State, 'T, 'F>)    

    type RWSRDelayed< 'Read, 'Write, 'State, 'T, 'F> = 
        unit -> RWSRInternal< 'Read, 'Write, 'State, 'T, 'F>

    [<NoComparison>]
    [<NoEquality>]
    type RWSResult< 'Read, 'Write, 'State, 'T, 'F> = RWSResult of RWSRDelayed< 'Read, 'Write, 'State, 'T, 'F>


    type RWSResultBuilder<'W> (monoid: IMonoid<'W>) = 
        member __.Bind(RWSResult delayed, f:'T -> RWSRInternal<'R,'W,'S,'U,'F>) : RWSRInternal<'R,'W,'S,'U,'F> =  
            RWSRInternal 
                (fun (state,read) -> 
                    let (RWSRInternal rwsResult1) = delayed ()
                    match rwsResult1 (state,read) with
                    | Success (_,write1,state1,value1) ->
                        let (RWSRInternal rwsResult2) = f value1
                        match rwsResult2 (state1,read) with
                        | Success (_,write2,state2,value2) ->
                            let writeCombined = monoid.Combine(write1,write2)
                            Success (read,writeCombined,state2,value2)
                        | Failure failure -> Failure failure
                    | Failure failure -> Failure failure
                )

        member __.Return(value:'T) : RWSRInternal<'R,'W,'S,'T,'F> = 
            RWSRInternal (fun (state,read) -> Success (read, monoid.Zero() , state ,value))

        member __.ReturnFrom(RWSResult update : RWSResult<'R,'W,'S,'T,'F>) : RWSRInternal<'R,'W,'S,'T,'F> = 
            update ()

        // member this.Yield(value:'T) : RWSResult<'R,'W,'S,'T> = this.Return value
        // member this.YieldFrom(delayed) = this.ReturnFrom delayed
        
        member this.Zero() : RWSRInternal<'R,'W,'S, unit,'F> = this.Return ()

        member __.Delay(f: RWSRDelayed<'R,'W,'S,'T,'F>) = f

        member __.Run(f:RWSRDelayed<'R,'W,'S,'T,'F>) = RWSResult f

        member this.Combine(rwsResult : RWSRInternal<'R,'W,'S,unit,'F>, delayed :RWSRDelayed<'R,'W,'S,'T,'F>) : RWSRInternal<'R,'W,'S,'T,'F> = 
            this.Bind(
                this.Run(
                    this.Delay(
                        fun () -> rwsResult)),
                delayed)

        member this.TryFinally(body:RWSRDelayed<'R,'W,'S,'T,'F>, compensation) =
            try this.ReturnFrom(RWSResult body)
            finally compensation() 

        member this.TryWith(body : RWSRDelayed<'R,'W,'S,'T,'F>, handler: 'F -> RWSRInternal<'R,'W,'S,'T,'F>) : RWSRInternal<'R,'W,'S,'T,'F> =

            let (RWSRInternal t) = this.ReturnFrom(RWSResult body)
            // let e =
            fun (read,state) ->
                let f = t (read,state)
                match f with
                | Success _ -> f
                | Failure failure -> 
                    try
                        let (RWSRInternal t1) = handler failure
                        t1 (read,state)
                    with
                    | :? System.Runtime.CompilerServices.RuntimeWrappedException ->
                        printfn "\n\n\n\n RuntimeWrappedException caught"    
                        f 
            |> RWSRInternal


           
        member this.Using(disposable:#System.IDisposable, body : 'a -> RWSRInternal<'R,'W,'S,'T,'F>) =
            let body' = fun () -> body disposable
            this.TryFinally(body', fun () -> 
                match disposable with 
                    | null -> () 
                    | disp -> disp.Dispose())

        member this.While(guard: unit -> bool, body: RWSRDelayed<'R,'W,'S,unit,'F>) : RWSRInternal<'R,'W,'S,unit,'F> =
            let rec whileLoop guard body =
                if guard() then 
                    this.Bind (RWSResult( body ), fun _ -> whileLoop guard body)
                else 
                    this.Zero()

            whileLoop guard body
    
        member this.For(sequence:seq<'a>, body: 'a -> RWSRInternal<'R,'W,'S,unit,'F>) : RWSRInternal<'R,'W,'S,unit,'F> =
            this.Using(sequence.GetEnumerator(),fun enum -> 
                this.While(enum.MoveNext, 
                    this.Delay(fun () -> body enum.Current)))            




    let rwsGetState (monoid : IMonoid<'W>) = 
        RWSResult(fun () -> RWSRInternal(fun (state,read) -> Success (read,monoid.Zero(),state,state) ) )

    let rwsPutState (monoid : IMonoid<'W>) (state: 'S) = 
        RWSResult(fun () -> RWSRInternal(fun (_,read) -> Success (read,monoid.Zero(),state,()) ) )

    let rwsRead (monoid : IMonoid<'W>) =
        RWSResult(fun () -> RWSRInternal(fun (state,read) -> Success (read,monoid.Zero(),state,read) ) )
        
    let rwsWrite (value : 'W) =
        RWSResult(fun () -> RWSRInternal(fun (state,read) -> Success (read,value,state,())) )
        
    let rwsRun (state,read) (RWSResult delayed: RWSResult<'R,'W,'S,'T,'F>) =
        let (RWSRInternal rwsResult) = delayed ()
        rwsResult (state,read)

       
        

