namespace Informedica.GenUnits.Lib

module UnitGroup =

    open MathNet.Numerics
    open Informedica.GenUnits.Lib
    open Informedica.GenUtils.Lib.BCL

    module CS = Constants
    module UN = Unit
    module CU = CombiUnit

    type UnitGroup = UnitGroup of string * (CU.Operator * string) list

    let create n = (n , []) |> UnitGroup

    let nameToString (n) = n

    let apply f (ug: UnitGroup) = ug |> f

    let get = apply id

    let getAll (UnitGroup(n, nl)) = n, nl 

    let addGroup o n ug = 
        let g, gl = ug |> getAll
        (g, [(o, n)] |> List.append gl) |> UnitGroup

    let perGroup = addGroup CU.Per

    let timesGroup = addGroup CU.Times

    let fromUnit cu = 
        let _, u, ul = cu |> CU.get
        (u |> UN.getGroupName, ul |> List.map (fun (op, _, u) -> op, u |> UN.getGroupName))
        |> UnitGroup

    let toString ug =
        let n, nl = ug |> getAll
        (n |> nameToString)::(nl |> List.map (fun (op, n) -> (op |> CU.opToString) + (n |> nameToString) ))
        |> String.concat ""

    let fromString s =
        let dels = "#"

        let rec parse ul usl =
            match usl with
            | [us] -> 
                let u = us
                (u, ul) |> UnitGroup
            | us::os::rest -> 
                let u = us
                let o = os |> CU.opFromString
                rest |> parse ([ (o, u)] @ ul)
            | _ -> failwith "Cannot parse string list"

        s
        |> String.replace CS.mults (dels + CS.mults + dels)
        |> String.replace CS.divs  (dels + CS.divs + dels)
        |> String.split dels
        |> List.rev
        |> parse []

    let eqs ug u = u |> fromUnit = ug

    /// Get all possible `CombiUnit` unit combinations
    /// belonging to a `UnitGroup` **ung**
    let getUnits ug =
        let n, nl = ug |> getAll

        let get n = 
            match UN.Units.units |> List.filter (fun us -> us.Group = n) with
            | [] -> [ (n |> Unit.Units.createGeneral).Unit ]
            | us -> us |> List.map Unit.Units.getUnit

        let us, usl = n |> get, nl |> List.map (fun (o, u) -> o, u |> get)
            
        let rec create usl cul =
            match usl with
            | [] -> cul
            | (o, ul)::tail ->
                let f =  match o with | CU.Per -> CU.per 1N | CU.Times -> CU.times 1N
                [
                    for cu in cul do
                        for u in ul do
                            yield cu |> f u
                ] |> create tail
            
        create usl (us |> List.map (fun u -> 1N |> CU.withUnit u))

    let calc op ug1 ug2 =  
        let cu1, cu2 = ug1 |> getUnits |> List.head, ug2 |> getUnits |> List.head
        (cu1 |> op <| cu2)
        |> fromUnit

    type UnitGroup with
        
        static member (*) (cu1, cu2) = calc (*) cu1 cu2

        static member (/) (cu1, cu2) = calc (/) cu1 cu2

        static member (+) (cu1, cu2) = calc (+) cu1 cu2

        static member (-) (cu1, cu2) = calc (-) cu1 cu2

