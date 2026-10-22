using System;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Collections.Generic;
var assembly = Assembly.LoadFrom(Path.GetFullPath(args[0]));
var lines = new List<string>();
foreach (var type in assembly.GetExportedTypes()) {
    lines.Add($"TYPE {type.FullName} {type.Attributes} {type.BaseType}");
    foreach (var face in type.GetInterfaces()) lines.Add($"INTERFACE {type.FullName} {face}");
    foreach (var member in type.GetMembers(BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static | BindingFlags.DeclaredOnly))
        lines.Add($"MEMBER {type.FullName} {member.MemberType} {member}");
}
lines.Sort(StringComparer.Ordinal);
File.WriteAllLines(args[1], lines);
Console.WriteLine($"{assembly.FullName}: {lines.Count} API entries");
var anon = assembly.GetType("Microsoft.FSharp.Linq.RuntimeHelpers.AnonymousObject`1").MakeGenericType(typeof(int));
var first = Activator.CreateInstance(anon, new object[] { 42 });
var second = Activator.CreateInstance(anon, new object[] { 42 });
Console.WriteLine($"AnonymousObject<int>(42) equals separate instance: {first.Equals(second)}; sealed: {anon.IsSealed}");
if (args.Length > 2 && first.Equals(second) != bool.Parse(args[2]))
    throw new InvalidOperationException("AnonymousObject equality differs from the expected runtime behavior.");
