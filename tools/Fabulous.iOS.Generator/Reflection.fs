namespace Fabulous.iOS.Generator

open Fabulous.CodeGen
open Fabulous.CodeGen.AssemblyReader.Models
open Fabulous.iOS.Generator.Helpers
open System
open System.IO
open System.Runtime.Loader

module Reflection =
    let loadAllAssemblies (paths: seq<string>) =
        let toFullPath p = Path.Combine(Environment.CurrentDirectory, p)
        
        paths
        |> Seq.map (toFullPath >> AssemblyLoadContext.Default.LoadFromAssemblyPath)
        |> Seq.toArray

    let isTypeResolvable name =
        true
    
    let tryGetPropertyInAssembly (assembly: System.Reflection.Assembly) (typeName, propertyName) =
        let toCleanTypeName propertyReturnType =
            propertyReturnType.ToString()
                              .Replace("[", "<")
                              .Replace("]", ">")
            |> Text.removeDotNetGenericNotation
        
        nullable {
            let! ``type`` = assembly.GetType(typeName)
            match ``type``.ContainsGenericParameters with
            | true -> return None // Generic types are not supported
            | false ->
                match ``type``.GetProperties() |> Seq.where (fun p -> p.Name = propertyName) |> Seq.tryHead with
                | None ->
                    return None
                | Some property ->
                    return
                        Some
                            { Name = propertyName
                              Type = property.PropertyType.FullName |> toCleanTypeName
                              DefaultValue = null }
        }
                        
                            
    let tryGetProperty (assemblies: System.Reflection.Assembly array) (typeName, propertyName) =
        assemblies
        |> Array.tryPick (fun asm -> tryGetPropertyInAssembly asm (typeName, propertyName))