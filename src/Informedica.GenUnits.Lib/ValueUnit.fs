namespace Informedica.GenUnits.Lib


module ValueUnit =

    open MathNet.Numerics

    open Informedica.GenUtils.Lib.BCL

    module US = Unit
    module CS = Constants
    module UN = Unit
    module CU = CombiUnit

    type ValueUnit = ValueUnit of BigRational * CU.CombiUnit


    let create v u = (v, u) |> ValueUnit


    let get (ValueUnit(v, u)) = v, u


    let calc op vu1 vu2 =
        let v1, u1 = vu1 |> get
        let v2, u2 = vu2 |> get
        let u = CU.calc op u1 u2
        let v = v1 |> CU.toBase u1 |> op <| (v2 |> CU.toBase u2) |> CU.toUnit u
        create v u


    let cmp cp vu1 vu2 =
        let v1, u1 = vu1 |> get
        let v2, u2 = vu2 |> get
        v1 |> CU.toBase u1 |> cp <| (v2 |> CU.toBase u2)


    let canConvert cu vu =
        let v, cu1 = vu |> get
        let _, u1, ul1 = cu1 |> CU.get
        let _, u2, ul2 = cu  |> CU.get

        let eq u1 u2 = u1 |> UN.getGroupName = (u2 |> UN.getGroupName)

        let canConvUl ul1 ul2 =
            ul1 |> List.forall2 (fun (o1, _, u1) (o2, _, u2) ->
                o1 = o2 && u1 |> eq u2
            ) ul2

        u1 |> eq u2 && canConvUl ul1 ul2     


    let convertTo cu vu =
        let v, cu1 = vu |> get
        let _, u1, ul1 = cu1 |> CU.get
        let _, u2, ul2 = cu  |> CU.get

        let v' = v |> CU.toBase cu1 |> CU.toUnit cu
        (v', cu) |> ValueUnit


    let toString vu =
        let v, u = vu |> get
        v.ToString() + " " + (u |> CU.toString)


    let toLangString lang prec vu =
        let v, u = vu |> get
        (v |> BigRational.toFloat 
           |> Double.fixPrecision prec
           |> string) + " " + (u |> CU.toLangString lang 1)


    let toFloatString prec vu =
        let v, u = vu |> get
        (v |> BigRational.toFloat |> Double.fixPrecision prec |> string) + " " + (u |> CU.toString)


    let fromString s =
        match s |> String.split CS.space with
        | vs::_ ->
            let v = vs |> BigRational.Parse
            let rest = s |> String.subString (vs |> String.length) ((s |> String.length) - (vs |> String.length))
            let cu = 
                rest
                |> String.trim
                |> CU.fromString
            (v, cu) |> ValueUnit
        | _ -> failwith "Cannot parse string"


    type ValueUnit with

        static member (*) (vu1, vu2) = calc (*) vu1 vu2

        static member (/) (vu1, vu2) = calc (/) vu1 vu2

        static member (+) (vu1, vu2) = calc (+) vu1 vu2

        static member (-) (vu1, vu2) = calc (-) vu1 vu2

        static member op_Equal (vu1, vu2) = cmp (=) vu1 vu2

        static member op_GreaterThan (vu1, vu2) = cmp (>) vu1 vu2

        static member op_SmallerThan (vu1, vu2) = cmp (<) vu1 vu2

        static member op_GreaterThanOrEqual (vu1, vu2) = cmp (>=) vu1 vu2

        static member op_SmallerThanOrEqual (vu1, vu2) = cmp (<=) vu1 vu2



    module Units = 

        type CU = CombiUnit

        let countTimes n v = CU.Units.countTimes n |> create v


        let massKiloGram n v = CU.Units.massKiloGram n |> create v


        let massGram n v = CU.Units.massGram n |> create v


        let massMilliGram n v = CU.Units.massMilliGram n |> create v


        let massMicroGram n v = CU.Units.massMicroGram n |> create v


        let massNanoGram n v  = CU.Units.massNanoGram n |> create v


        let volumeLiter n v = CU.Units.volumeLiter n |> create v


        let volumeDeciLiter n v = CU.Units.volumeDeciLiter n |> create v

         
        let volumeMilliLiter n v = CU.Units.volumeMilliLiter n |> create v

         
        let volumeMicroLiter n v = CU.Units.volumeMicroLiter n |> create v


        let timeYear n v = CU.Units.timeYear n |> create v


        let timeMonth n v = CU.Units.timeMonth n |> create v


        let timeWeek n v = CU.Units.timeWeek n |> create v


        let timeDay n v = CU.Units.timeDay n |> create v


        let timeHour n v = CU.Units.timeHour n |> create v


        let timeMinute n v = CU.Units.timeMinute n |> create v

         
        let timeSecond n v = CU.Units.timeSecond n |> create v


        let molarMol n v = CU.Units.molarMol n |> create v


        let molarMilliMol n v = CU.Units.molarMilliMol n |> create v

                         
        let weightKg n v = CU.Units.weightKg n |> create v


        let weightGram n v = CU.Units.weightGram n |> create v


        let bsaM2 n v = CU.Units.bsaM2 n |> create v

