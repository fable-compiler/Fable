AST: [MemberDeclaration
   { Name = "hello"
     FullDisplayName = "QuickTest.hello"
     Args = []
     Body =
      Call
        (Import
           ({ Selector = "toConsole"
              Path =
               "./.fable/fable-library.3.0.0-local-build-20210819-1229/String.js"
              Kind = LibraryImport }, Any, None),
         { ThisArg = None
           Args =
            [TypeCast
               (Call
                  (Import
                     ({ Selector = "printf"
                        Path =
                         "./.fable/fable-library.3.0.0-local-build-20210819-1229/String.js"
                        Kind = LibraryImport }, Any, None),
                   { ThisArg = None
                     Args =
                      [Value
                         (StringConstant "hello world",
                          Some { start = { line = 17
                                           column = 20 }
                                 end = { line = 17
                                         column = 33 }
                                 identifierName = None })]
                     SignatureArgTypes = [String]
                     CallMemberInfo = None
                     HasSpread = false
                     IsConstructor = false
                     OptimizableInto = None },
                   DeclaredType
                     ({ FullName = "Microsoft.FSharp.Core.PrintfFormat`5"
                        Path =
                         AssemblyPath
                           "C:/Users/metal/.nuget/packages/fsharp.core/5.0.1/lib/netstandard2.0/FSharp.Core.dll" },
                      [Unit;
                       DeclaredType
                         ({ FullName = "System.IO.TextWriter"
                            Path = CoreAssemblyName "System.Runtime" }, []);
                       Unit; Unit; Unit]), Some { start = { line = 17
                                                            column = 20 }
                                                  end = { line = 17
                                                          column = 33 }
                                                  identifierName = None }),
                DeclaredType
                  ({ FullName = "Microsoft.FSharp.Core.PrintfFormat`4"
                     Path =
                      AssemblyPath
                        "C:/Users/metal/.nuget/packages/fsharp.core/5.0.1/lib/netstandard2.0/FSharp.Core.dll" },
                   [Unit;
                    DeclaredType
                      ({ FullName = "System.IO.TextWriter"
                         Path = CoreAssemblyName "System.Runtime" }, []); Unit;
                    Unit]))]
           SignatureArgTypes =
            [DeclaredType
               ({ FullName = "Microsoft.FSharp.Core.PrintfFormat`4"
                  Path =
                   AssemblyPath
                     "C:/Users/metal/.nuget/packages/fsharp.core/5.0.1/lib/netstandard2.0/FSharp.Core.dll" },
                [GenericParam ("T", []);
                 DeclaredType ({ FullName = "System.IO.TextWriter"
                                 Path = CoreAssemblyName "System.Runtime" }, []);
                 Unit; Unit])]
           CallMemberInfo = None
           HasSpread = false
           IsConstructor = false
           OptimizableInto = None }, Unit, Some { start = { line = 17
                                                            column = 12 }
                                                  end = { line = 17
                                                          column = 33 }
                                                  identifierName = None })
     Info = Fable.Transforms.FSharp2Fable.MemberInfo
     UsedNames = set []
     ExportDefault = false };
 MemberDeclaration
   { Name = "a"
     FullDisplayName = "QuickTest.a"
     Args = []
     Body =
      Operation
        (Binary
           (BinaryPlus,
            Value
              (NumberConstant (2.0, Int32, None), Some { start = { line = 18
                                                                   column = 8 }
                                                         end = { line = 18
                                                                 column = 9 }
                                                         identifierName = None }),
            Value
              (NumberConstant (2.0, Int32, None), Some { start = { line = 18
                                                                   column = 12 }
                                                         end = { line = 18
                                                                 column = 13 }
                                                         identifierName = None })),
         Number (Int32, None), Some { start = { line = 18
                                                column = 8 }
                                      end = { line = 18
                                              column = 13 }
                                      identifierName = None })
     Info = Fable.Transforms.FSharp2Fable.MemberInfo
     UsedNames = set ["arg0_0"; "arg1_0"]
     ExportDefault = false };
 MemberDeclaration
   { Name = "b"
     FullDisplayName = "QuickTest.b"
     Args = []
     Body =
      Operation
        (Binary
           (BinaryMinus,
            Value
              (NumberConstant (3.0, Int32, None), Some { start = { line = 19
                                                                   column = 8 }
                                                         end = { line = 19
                                                                 column = 9 }
                                                         identifierName = None }),
            Value
              (NumberConstant (1.0, Int32, None), Some { start = { line = 19
                                                                   column = 12 }
                                                         end = { line = 19
                                                                 column = 13 }
                                                         identifierName = None })),
         Number (Int32, None), Some { start = { line = 19
                                                column = 8 }
                                      end = { line = 19
                                              column = 13 }
                                      identifierName = None })
     Info = Fable.Transforms.FSharp2Fable.MemberInfo
     UsedNames = set ["arg0_0"; "arg1_0"]
     ExportDefault = false };
 MemberDeclaration
   { Name = "c"
     FullDisplayName = "QuickTest.c"
     Args = []
     Body =
      Operation
        (Binary
           (BinaryPlus, IdentExpr { Name = "a"
                                    Type = Number (Int32, None)
                                    IsMutable = false
                                    IsThisArgument = false
                                    IsCompilerGenerated = true
                                    Range = Some { start = { line = 20
                                                             column = 8 }
                                                   end = { line = 20
                                                           column = 9 }
                                                   identifierName = Some "a" } },
            IdentExpr { Name = "b"
                        Type = Number (Int32, None)
                        IsMutable = false
                        IsThisArgument = false
                        IsCompilerGenerated = true
                        Range = Some { start = { line = 20
                                                 column = 12 }
                                       end = { line = 20
                                               column = 13 }
                                       identifierName = Some "b" } }),
         Number (Int32, None), Some { start = { line = 20
                                                column = 8 }
                                      end = { line = 20
                                              column = 13 }
                                      identifierName = None })
     Info = Fable.Transforms.FSharp2Fable.MemberInfo
     UsedNames = set ["arg0_0"; "arg1_0"]
     ExportDefault = false };
 MemberDeclaration
   { Name = "execute"
     FullDisplayName = "QuickTest.execute"
     Args = [{ Name = "unitVar0"
               Type = Unit
               IsMutable = false
               IsThisArgument = false
               IsCompilerGenerated = true
               Range = Some { start = { line = 21
                                        column = 12 }
                              end = { line = 21
                                      column = 14 }
                              identifierName = Some "unitVar0" } }]
     Body =
      Extended
        (Return
           (Sequential
              [Let
                 ({ Name = "arg10"
                    Type = String
                    IsMutable = false
                    IsThisArgument = false
                    IsCompilerGenerated = true
                    Range = Some { start = { line = 22
                                             column = 20 }
                                   end = { line = 22
                                           column = 31 }
                                   identifierName = Some "arg10" } },
                  Let
                    ({ Name = "copyOfStruct"
                       Type = Number (Int32, None)
                       IsMutable = true
                       IsThisArgument = false
                       IsCompilerGenerated = true
                       Range = Some { start = { line = 22
                                                column = 4 }
                                      end = { line = 22
                                              column = 16 }
                                      identifierName = Some "copyOfStruct" } },
                     IdentExpr { Name = "a"
                                 Type = Number (Int32, None)
                                 IsMutable = false
                                 IsThisArgument = false
                                 IsCompilerGenerated = true
                                 Range = Some { start = { line = 22
                                                          column = 4 }
                                                end = { line = 22
                                                        column = 5 }
                                                identifierName = Some "a" } },
                     Call
                       (Import
                          ({ Selector = "int32ToString"
                             Path =
                              "./.fable/fable-library.3.0.0-local-build-20210819-1229/Util.js"
                             Kind = LibraryImport }, Any, None),
                        { ThisArg = None
                          Args =
                           [IdentExpr
                              { Name = "copyOfStruct"
                                Type = Number (Int32, None)
                                IsMutable = true
                                IsThisArgument = false
                                IsCompilerGenerated = true
                                Range =
                                 Some { start = { line = 22
                                                  column = 4 }
                                        end = { line = 22
                                                column = 16 }
                                        identifierName = Some "copyOfStruct" } }]
                          SignatureArgTypes = []
                          CallMemberInfo = None
                          HasSpread = false
                          IsConstructor = false
                          OptimizableInto = None }, String, None)),
                  CurriedApply
                    (Call
                       (Import
                          ({ Selector = "toConsole"
                             Path =
                              "./.fable/fable-library.3.0.0-local-build-20210819-1229/String.js"
                             Kind = LibraryImport }, Any, None),
                        { ThisArg = None
                          Args =
                           [TypeCast
                              (Call
                                 (Import
                                    ({ Selector = "printf"
                                       Path =
                                        "./.fable/fable-library.3.0.0-local-build-20210819-1229/String.js"
                                       Kind = LibraryImport }, Any, None),
                                  { ThisArg = None
                                    Args =
                                     [Value
                                        (StringConstant "%s",
                                         Some { start = { line = 22
                                                          column = 27 }
                                                end = { line = 22
                                                        column = 31 }
                                                identifierName = None })]
                                    SignatureArgTypes = [String]
                                    CallMemberInfo = None
                                    HasSpread = false
                                    IsConstructor = false
                                    OptimizableInto = None },
                                  DeclaredType
                                    ({ FullName =
                                        "Microsoft.FSharp.Core.PrintfFormat`5"
                                       Path =
                                        AssemblyPath
                                          "C:/Users/metal/.nuget/packages/fsharp.core/5.0.1/lib/netstandard2.0/FSharp.Core.dll" },
                                     [LambdaType (String, Unit);
                                      DeclaredType
                                        ({ FullName = "System.IO.TextWriter"
                                           Path =
                                            CoreAssemblyName "System.Runtime" },
                                         []); Unit; Unit; String]),
                                  Some { start = { line = 22
                                                   column = 27 }
                                         end = { line = 22
                                                 column = 31 }
                                         identifierName = None }),
                               DeclaredType
                                 ({ FullName =
                                     "Microsoft.FSharp.Core.PrintfFormat`4"
                                    Path =
                                     AssemblyPath
                                       "C:/Users/metal/.nuget/packages/fsharp.core/5.0.1/lib/netstandard2.0/FSharp.Core.dll" },
                                  [LambdaType (String, Unit);
                                   DeclaredType
                                     ({ FullName = "System.IO.TextWriter"
                                        Path = CoreAssemblyName "System.Runtime" },
                                      []); Unit; Unit]))]
                          SignatureArgTypes =
                           [DeclaredType
                              ({ FullName =
                                  "Microsoft.FSharp.Core.PrintfFormat`4"
                                 Path =
                                  AssemblyPath
                                    "C:/Users/metal/.nuget/packages/fsharp.core/5.0.1/lib/netstandard2.0/FSharp.Core.dll" },
                               [GenericParam ("T", []);
                                DeclaredType
                                  ({ FullName = "System.IO.TextWriter"
                                     Path = CoreAssemblyName "System.Runtime" },
                                   []); Unit; Unit])]
                          CallMemberInfo = None
                          HasSpread = false
                          IsConstructor = false
                          OptimizableInto = None }, LambdaType (String, Unit),
                        Some { start = { line = 22
                                         column = 20 }
                               end = { line = 22
                                       column = 31 }
                               identifierName = None }),
                     [IdentExpr
                        { Name = "arg10"
                          Type = String
                          IsMutable = false
                          IsThisArgument = false
                          IsCompilerGenerated = true
                          Range = Some { start = { line = 22
                                                   column = 20 }
                                         end = { line = 22
                                                 column = 31 }
                                         identifierName = Some "arg10" } }],
                     Unit, Some { start = { line = 22
                                            column = 20 }
                                  end = { line = 22
                                          column = 31 }
                                  identifierName = None }));
               Call
                 (Import
                    ({ Selector = "toConsole"
                       Path =
                        "./.fable/fable-library.3.0.0-local-build-20210819-1229/String.js"
                       Kind = LibraryImport }, Any, None),
                  { ThisArg = None
                    Args =
                     [TypeCast
                        (Call
                           (Import
                              ({ Selector = "printf"
                                 Path =
                                  "./.fable/fable-library.3.0.0-local-build-20210819-1229/String.js"
                                 Kind = LibraryImport }, Any, None),
                            { ThisArg = None
                              Args =
                               [Value
                                  (StringConstant "c",
                                   Some { start = { line = 23
                                                    column = 11 }
                                          end = { line = 23
                                                  column = 14 }
                                          identifierName = None })]
                              SignatureArgTypes = [String]
                              CallMemberInfo = None
                              HasSpread = false
                              IsConstructor = false
                              OptimizableInto = None },
                            DeclaredType
                              ({ FullName =
                                  "Microsoft.FSharp.Core.PrintfFormat`5"
                                 Path =
                                  AssemblyPath
                                    "C:/Users/metal/.nuget/packages/fsharp.core/5.0.1/lib/netstandard2.0/FSharp.Core.dll" },
                               [Unit;
                                DeclaredType
                                  ({ FullName = "System.IO.TextWriter"
                                     Path = CoreAssemblyName "System.Runtime" },
                                   []); Unit; Unit; Unit]),
                            Some { start = { line = 23
                                             column = 11 }
                                   end = { line = 23
                                           column = 14 }
                                   identifierName = None }),
                         DeclaredType
                           ({ FullName = "Microsoft.FSharp.Core.PrintfFormat`4"
                              Path =
                               AssemblyPath
                                 "C:/Users/metal/.nuget/packages/fsharp.core/5.0.1/lib/netstandard2.0/FSharp.Core.dll" },
                            [Unit;
                             DeclaredType
                               ({ FullName = "System.IO.TextWriter"
                                  Path = CoreAssemblyName "System.Runtime" }, []);
                             Unit; Unit]))]
                    SignatureArgTypes =
                     [DeclaredType
                        ({ FullName = "Microsoft.FSharp.Core.PrintfFormat`4"
                           Path =
                            AssemblyPath
                              "C:/Users/metal/.nuget/packages/fsharp.core/5.0.1/lib/netstandard2.0/FSharp.Core.dll" },
                         [GenericParam ("T", []);
                          DeclaredType
                            ({ FullName = "System.IO.TextWriter"
                               Path = CoreAssemblyName "System.Runtime" }, []);
                          Unit; Unit])]
                    CallMemberInfo = None
                    HasSpread = false
                    IsConstructor = false
                    OptimizableInto = None }, Unit,
                  Some { start = { line = 23
                                   column = 4 }
                         end = { line = 23
                                 column = 14 }
                         identifierName = None })]), None)
     Info = Fable.Transforms.FSharp2Fable.MemberInfo
     UsedNames = set ["arg10"; "clo1"; "copyOfStruct"; "unitVar0"]
     ExportDefault = false }]local mod = {}
function mod.hello () 
    print(("hello world"))
end
function mod.a () 
    2 + 2
end
function mod.b () 
    3 - 1
end
function mod.c () 
    a + b
end
function mod.execute (unitVar0) 
    return (function ()

        arg10 = copyOfStruct = a
        require("./.fable/fable-library.3.0.0-local-build-20210819-1229/Util.js").int32ToString(copyOfStruct)
        print(("%s"))(arg10)
        return print(("c"))

    end)()
end
return mod
