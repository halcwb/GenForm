namespace Informedica.GenUnits.Lib



module CombiUnit =

    open MathNet.Numerics

    open Informedica.GenUtils.Lib.BCL

    type CombiUnit = 
    | Combi of BigRational * Unit.Unit * (Operator * BigRational * Unit.Unit) list
    and Operator =
    | Per
    | Times

            
    let create v u = (v , u, []) |> Combi


    let get (Combi(v, u, ul)) = v, u, ul


    let operator op v u vu =
        let v', u', ul = vu |> get
        (v', u', ul @ [(op, v, u)]) 
        |> Combi


    let withUnit u v = create v u


    let per = operator Per
   
     
    let times = operator Times


    let getMultiplier vu = 
        let v, u, ul = vu |> get
        let mp v u = v * (u |> Unit.getMultiplier)

        ul 
        |> List.fold (fun acc (op, v, u) ->
            match op with
            | Per   -> acc / (mp v u)
            | Times -> acc * (mp v u)) (mp v u)

                        
    let toBase u v = v |> Unit.Multipliers.toBase (u |> getMultiplier)


    let toUnit u v = v |> Unit.Multipliers.toUnit (u |> getMultiplier)


    let eqGroup cu1 cu2 =
        let _, u1, ul1 = cu1 |> get
        let _, u2, ul2 = cu2 |> get
        u1 |> Unit.eqGroup u2 && 
        ul1 
        |> List.forall2 (fun (_, _, u1) (_, _, u2) -> u1 |> Unit.eqGroup u2) ul2


    let eval x =
        let _, u, ul = x |> get

        let sort xs =
            xs |> List.sortWith(fun x1 x2 -> 
                let op1, v1, _ = x1
                let op2, v2, _ = x2 
                match op1, op2 with
                | Times, Times -> if v1 > v2 then -1 else 0
                | Times, Per   -> -1
                | Per,  Times  -> +1
                | Per,  Per    -> 0)

        let eqs x1 x2 =
            let op1, _, u1 = x1
            let op2, _, u2 = x2
            let opeq = op1 = Per || op2 = Per
            let greq = u1 |> Unit.eqGroup u2
            opeq && greq 

        let rec simplify acc list = 
            let remCount xs = 
                xs 
                |> List.filter(fun x -> 
                    let (_, _, u) = x
                    u |> Unit.isCount |> not) 
                
            let rec remove i l =
                match i, l with
                | 0, x::xs -> xs
                | i, x::xs -> x::remove (i - 1) xs
                | i, [] -> failwith "index out of range"

            match list with
            | [] -> 
                let acc = acc |> remCount |> sort
                match acc with
                | [(Per, _, _)] -> (Times, 1N, Unit.count)::acc
                | _             -> acc
            | x::xs -> 
                match xs |> List.tryFindIndex (eqs x) with
                | Some i -> 
                    xs |> remove i |> simplify acc
                | None -> xs |> simplify (acc @ [x])
                    
        match simplify [] ((Times, 1N, u)::ul) with
        | [] -> create 1N Unit.count
        | x::xs -> 
            let _, v, u = x
            (v, u, xs) |> Combi


    let (|Mult|Div|Add|Subtr|) op =
        match op with
        | _ when 1N |> op <| 2N = 2N      -> Mult
        | _ when 1N |> op <| 2N = (1N/2N) -> Div
        | _ when 1N |> op <| 2N = 3N      -> Add
        | _ when 1N |> op <| 2N = -1N     -> Subtr
        | _ -> failwith "Not a valid operator"


    let calc op cu1 cu2 = 
        let toOp op = 
            match op with
            | Mult -> Times
            | Div  -> Per
            | _ -> failwith "Not a valid unit operator"

        let v1, u1, ul1 = cu1 |> get
        let v2, u2, ul2 = cu2 |> get

        match op with
        | Mult | Div ->
            (v1, u1, ul1 @ [op |> toOp, v2, u2] @ ul2) 
            |> Combi
            |> eval
        | Add | Subtr -> 
            if cu1 |> eqGroup cu2 then cu2
            else failwith "Cannot add units with different unit groups"


    let opToString = function
        | Per   -> Constants.divs
        | Times -> Constants.mults

    let opFromString s =
        match s with
        | _ when s = Constants.mults -> Times
        | _ when s = Constants.divs  -> Per
        | _ -> failwith "Not a valid operator string"


    let toString cu =
        let abbr = Unit.getAbbreviation
        let gr u = u |> Unit.getGroupName 
        let toStr u = (u |> abbr) + Constants.openBr + (u |> gr) + Constants.closBr

        let bigRatToString (v: BigRational) =
            if v = 1N then Constants.empts else v.ToString()

        let v, u, ul = cu |> get
        let acc = (v |> bigRatToString) + Constants.space + (u |> toStr) |> String.trim
        ul 
        |> List.fold (fun acc (o, v, u) -> 
                let v' = v |> bigRatToString
                let o' = o |> opToString
                let u' = u |> toStr
                acc +
                if v' = Constants.empts then o' + u' else o' + v' + Constants.space + u') acc


    let fromString s =
        let dels = "#"
        let getUnitAndGroup ug = 
            match ug |> String.replace Constants.closBr Constants.empts |> String.split Constants.openBr with
            | [u;g] -> u, g
            | _ -> sprintf "Could not parse unit from string: %s" ug |> failwith

        let ufs s =
            match s |> String.split Constants.space with
            | [ug] ->
                let u, g = ug |> getUnitAndGroup 
                match Unit.fromString u g with
                | Some (u') -> 1N, u'
                | None     -> failwith "Not a valid unit"
            | [v;ug] -> 
                let u, g = ug |> getUnitAndGroup 
                let v' = v |> BigRational.Parse
                match Unit.fromString u g with
                | Some (u') -> v', u'
                | None     -> failwith "Not a valid unit"
            | _ -> failwith "Cannot parse string"

        let rec parse ul usl =
            match usl with
            | [us] -> 
                let v, u = us |> ufs
                (v, u, ul) |> Combi
            | us::os::rest -> 
                let v, u = us |> ufs
                let o = os |> opFromString
                rest |> parse ([ (o, v, u)] @ ul)
            | _ -> failwith "Cannot parse string list"

        s 
        |> String.replace Constants.mults (dels + Constants.mults + dels) 
        |> String.replace Constants.divs  (dels + Constants.divs + dels)
        |> String.split dels
        |> List.rev
        |> parse []
        

    let toLangString lang prec cu =
        let toStr u = Unit.toLangString lang u

        let bigRatToString (v: BigRational) =
            if v = 1N then Constants.empts else v |> BigRational.toFloat |> Double.fixPrecision prec |> string

        let v, u, ul = cu |> get
        let acc = (v |> bigRatToString) + Constants.space + (u |> toStr) |> String.trim
        ul 
        |> List.fold (fun acc (o, v, u) -> 
                let v' = v |> bigRatToString
                let o' = o |> opToString
                let u' = u |> toStr
                acc +
                if v' = Constants.empts then o' + u' else o' + v' + Constants.space + u') acc


    type CombiUnit with
        
        static member (*) (cu1, cu2) = calc (*) cu1 cu2

        static member (/) (cu1, cu2) = calc (/) cu1 cu2

        static member (+) (cu1, cu2) = calc (+) cu1 cu2

        static member (-) (cu1, cu2) = calc (-) cu1 cu2


    module Units = 

        open Unit 

        let countTimes n = Count Times |> create n


        let massKiloGram n = Mass KiloGram |> create n


        let massGram n = Mass Gram |> create n


        let massMilliGram n = Mass MilliGram |> create n


        let massMicroGram n = Mass MicroGram |> create n


        let massNanoGram n = Mass NanoGram |> create n


        let volumeLiter n = Volume Liter |> create n


        let volumeDeciLiter n = Volume DeciLiter |> create n

         
        let volumeMilliLiter n = Volume MilliLiter |> create n

         
        let volumeMicroLiter n = Volume MicroLiter |> create n


        let timeYear n = Time Year |> create n


        let timeMonth n = Time Month |> create n


        let timeWeek n = Time Week |> create n


        let timeDay n = Time Day |> create n


        let timeHour n = Time Hour |> create n


        let timeMinute n = Time Minute |> create n

         
        let timeSecond n = Time Second |> create n


        let molarMol n = Molar Mol |> create n


        let molarMilliMol n = Molar MilliMol |> create n

                         
        let weightKg n = Weight WeightKilogram |> create n


        let weightGram n = Weight WeightGram |> create n


        let bsaM2 n = BSA M2 |> create n



