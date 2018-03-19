namespace Informedica.GenUtils.Lib

module Memoization =

    open System.Collections.Generic
    
    let memoize f =
        let cache = ref Map.empty
        fun x ->
            match (!cache).TryFind(x) with
            | Some r -> r
            | None ->
                let r = f x
                cache := (!cache).Add(x, r)
                r

    let memoizeOne f = 
        let dic = new Dictionary<_, _>()
        let memoized par =
            if dic.ContainsKey(par) then 
                dic.[par]
            else
                let result = f par
                dic.Add(par, result)
                result

        memoized

    let memoize2Int f =
        let dic = new Dictionary<int * int, _>()
        let memoized p1 p2 =
            let hash = p1.GetHashCode(), p2.GetHashCode()
            if dic.ContainsKey(hash) then 
                dic.[hash]
            else
                let result = f p1 p2
                dic.Add(hash, result)
                result

        memoized
