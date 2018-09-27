#time

type Unit = string


/// Value and unit
type ValueUnit =
    {
        Value : float
        Unit : Unit
    }


/// Range with min and/or max
type MinMax =
    // No range
    | None
    // Only a min
    | Min of ValueUnit
    // Only a max
    | Max of ValueUnit
    // A min and max 
    | MinMax of ValueUnit

/// Dose limits
type DoseRange =
    {
        // Normal limits
        Norm : MinMax
        // Normal limits adjusted by weight
        NormWeight : MinMax
        // Normal limits adjusted by BSA
        NormBSA : MinMax
        // Absolute limits
        Abs : MinMax
        // Absolute limits adjusted by weight
        AbsWeight : MinMax
        // Absolute limits adjusted by BSA
        AbsBSA : MinMax
    }



/// Dosage
type Dosage =
    {
        /// Dosage at the start
        StartDosage : DoseRange
        /// Dosage per administration
        AdminDosage : DoseRange
        /// Dosage rate
        RateDosage : DoseRange * RateUnit
        /// Total dosage per time period
        TotalDosage : Frequencies * TimeUnit * DoseRange list
    }
and Frequencies = int list
and TimeUnit = Unit
and RateUnit = Unit
    


/// Substance
type SubstanceDosage =
    {
        Name : string
        Dosage : Dosage
    }

/// Doserule
type DoseRule =
    {   
        // Generic the doserule applies to
        Generic : string
        // ATCTherapyGroup the doserule applies to
        ATCTherapyGroup : string
        // ATCTherapySubGroup the doserule applies to
        ATCTherapySubGroup : string
        // TradeProducts the doserule applies to
        TradeProducts : string list
        // GenericProducts the doserule applies to
        GenericProducts : string list
        // Indication the doserule applies to
        IndicationDosages : IndicationDosage list
    }
and IndicationDosage =
    {
        Indications : string list
        RouteDosage : RouteDosage list
    }
and RouteDosage =
    {
        Route : string
        PatientDosages : PatientDosage list
    } 
and PatientDosage =
    {
        Patient : Patient
        // Dosage of the shape
        ShapeDosage : ShapeDosage
        // List of substances that have a dosage
        SubstDosages : SubstanceDosage list
    }
and Patient =
    {
        GestAge : MinMax
        Age : MinMax
        Weight : MinMax
        BSA : MinMax
    }
and ShapeDosage =
    {
        Shape : string
        Dosage : Dosage
    }
