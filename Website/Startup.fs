namespace Solitaire.Website

open System
open System.Net.Http
open Microsoft.AspNetCore.Components.WebAssembly.Hosting
open Microsoft.Extensions.DependencyInjection

module Program =

  [<EntryPoint>]
  let Main args =
    let builder = WebAssemblyHostBuilder.CreateDefault(args)
    builder.Services.AddScoped<HttpClient>(
      fun _ -> new HttpClient(BaseAddress = Uri builder.HostEnvironment.BaseAddress)
      ) 
      |> ignore
    builder.RootComponents.Add<Main.MyApp>("#main")
    builder.Build().RunAsync() |> ignore
    0