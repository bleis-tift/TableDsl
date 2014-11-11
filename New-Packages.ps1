C:\Windows\Microsoft.NET\Framework64\v4.0.30319\MSBuild.exe TableDsl.sln /property:Configuration=Release /property:VisualStudioVersion=12.0 /target:rebuild

cd TableDsl.Core
..\.nuget\nuget.exe pack TableDsl.Core.fsproj -Build -Symbols -Properties VisualStudioVersion=12.0
cd ..\TableDsl.Sql
..\.nuget\nuget.exe pack TableDsl.Sql.fsproj -Build -Symbols -Properties VisualStudioVersion=12.0
cd ..\TableDsl
..\.nuget\nuget.exe pack TableDsl.fsproj -Properties VisualStudioVersion=12.0

cd ..

mkdir nuget-packages
mv TableDsl.Core\*.nupkg nuget-packages
mv TableDsl.Sql\*.nupkg nuget-packages
mv TableDsl\*.nupkg nuget-packages

cd nuget-packages

ls *.nupkg | %{
  echo "..\.nuget\nuget.exe push $_" >> Push-All.ps1
}

cd ..
