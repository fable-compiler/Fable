yarn build
node out/bench/app.js \
    ../metadata/ \
    test_script.fsx \
    out/test_script.js

# To test .NET use the following instead
# dotnet run -c Release -p platform.net \
#     ../metadata/ \
#     test_script.fsx \
#     out/test_script.js
