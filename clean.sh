rm -r src/Informedica.GenUtils.Lib/bin/
rm -r src/Informedica.GenUtils.Lib/obj/

rm -r src/Informedica.GenUnits.Lib/bin/
rm -r src/Informedica.GenUnits.Lib/obj/

rm -r src/Informedica.GenProduct.Lib/bin/
rm -r src/Informedica.GenProduct.Lib/obj/

rm -r tests/Informedica.GenUtils.Tests/bin/
rm -r tests/Informedica.GenUtils.Tests/obj/

rm -r tests/Informedica.GenUnits.Tests/bin/
rm -r tests/Informedica.GenUnits.Tests/obj/

rm -r tests/Informedica.GenProduct.Tests/bin/
rm -r tests/Informedica.GenProduct.Tests/obj/

# rm -r packages/
# rm -r paket-files/

# rm paket.lock
# rm .paket/Paket.Restore.targets

mono .paket/paket.exe install

dotnet restore
dotnet build
