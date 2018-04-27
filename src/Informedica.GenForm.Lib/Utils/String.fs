namespace Informedica.GenForm.Lib

module String =

    open Informedica.GenUtils.Lib.BCL


    let removeTextBetween start stop text =
        let regex = @"\" + start + @"[^\" + stop + "]*]"
        printfn "regex: %s" regex

        (String.regex regex).Replace(text, "")
        |> String.trim


    let removeTextBetweenBrackets = removeTextBetween "[" "]"
        
  
