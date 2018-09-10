#load "references.fsx"

#time



module Tests =

    open MathNet.Numerics
    open Informedica.GenUnits.Lib

    open ValueUnit

    let toString = toString Units.English Units.Short

    // Print intermediate results
    let (>>*) u f =
        u |> toString |> printfn "%s"
        f u

    // Some basic units
    let mg400 = 400N |> create Units.Mass.milliGram
    let ml50  = 50N |> create Units.Volume.milliLiter
    let l5 = 5N |> create Units.Volume.liter 

    // Get the base value of the units
    ml50 |> toBase
    l5 |> toBase
    // Normal comparison doesn't work
    (ml50 > l5) // Returns true, but is false
    // Use the specific comparison
    (ml50 >? l5) // Returns correct true

    // Calculation with units
    ((mg400 + mg400)/ ml50)     // 16 mg[Mass]/ml[Volume]
    >>* ((*) ml50)              // 800 mg[Mass] 
    >>* (fun vu -> vu / ml50)   // 16 mg[Mass]/ml[Volume]
    >>* ((*) ml50)              // 800 mg[Mass]
    |> toString

    // Unit conversion
    l5                            // 5 l[Volume]
    ==> Units.Volume.milliLiter   // = 5000 ml[Volume]
    |> toString

    // Calculate and get the resulting unit group
    4N
    |> create (Units.General.general "dose") // 4 dose[General]
    >>* (fun vu -> vu / (1N |> create Units.Time.day)) // divide by 1 day[Time]
    >>* (fun vu -> vu ==> (Units.General.general "dose" |> per (Units.Time.week)))
    |> (fun (ValueUnit(_, u)) ->
        u |> Group.unitToGroup
    ) // GeneralGroup "dose", OpPer, TimeGroup -> i.e. Dose/Time

    // Calculate and get all valid units for conversion
    mg400 / ml50 / (1N |> create Units.Time.day) // 8 mg[Mass]/ml[Volume]/day[Time]
    >>* (fun vu -> 
        let (_, u) = vu |> get
        u 
        |> Group.unitToGroup
        |> Group.getUnits
        |> List.iter (fun u ->
            create u 1N
            |> toString
            |> printfn "%s"
        )
    )

    // Prints out:
    //1 kg[Mass]/l[Volume]/yr[Time]
    //1 kg[Mass]/l[Volume]/mo[Time]
    //1 kg[Mass]/l[Volume]/wk[Time]
    //1 kg[Mass]/l[Volume]/day[Time]
    //1 kg[Mass]/l[Volume]/hr[Time]
    //1 kg[Mass]/l[Volume]/min[Time]
    //1 kg[Mass]/l[Volume]/sec[Time]
    //1 kg[Mass]/dl[Volume]/yr[Time]
    //1 kg[Mass]/dl[Volume]/mo[Time]
    //1 kg[Mass]/dl[Volume]/wk[Time]
    //1 kg[Mass]/dl[Volume]/day[Time]
    //1 kg[Mass]/dl[Volume]/hr[Time]
    //1 kg[Mass]/dl[Volume]/min[Time]
    //1 kg[Mass]/dl[Volume]/sec[Time]
    //1 kg[Mass]/ml[Volume]/yr[Time]
    //1 kg[Mass]/ml[Volume]/mo[Time]
    //1 kg[Mass]/ml[Volume]/wk[Time]
    //1 kg[Mass]/ml[Volume]/day[Time]
    //1 kg[Mass]/ml[Volume]/hr[Time]
    //1 kg[Mass]/ml[Volume]/min[Time]
    //1 kg[Mass]/ml[Volume]/sec[Time]
    //1 kg[Mass]/microl[Volume]/yr[Time]
    //1 kg[Mass]/microl[Volume]/mo[Time]
    //1 kg[Mass]/microl[Volume]/wk[Time]
    //1 kg[Mass]/microl[Volume]/day[Time]
    //1 kg[Mass]/microl[Volume]/hr[Time]
    //1 kg[Mass]/microl[Volume]/min[Time]
    //1 kg[Mass]/microl[Volume]/sec[Time]
    //1 g[Mass]/l[Volume]/yr[Time]
    //1 g[Mass]/l[Volume]/mo[Time]
    //1 g[Mass]/l[Volume]/wk[Time]
    //1 g[Mass]/l[Volume]/day[Time]
    //1 g[Mass]/l[Volume]/hr[Time]
    //1 g[Mass]/l[Volume]/min[Time]
    //1 g[Mass]/l[Volume]/sec[Time]
    // etc ...