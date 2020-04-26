namespace FableBounce

open Fable.Core
open Fable.Core.JsInterop
open Config
open Browser.Types

type Particle =
  { Coord: Vector2
    Velocity: Vector2
    Radius: float
    Color: string }

module Particle =
  let palette = [| "red"; "green"; "orange"; "darkgray"; "teal" |]

  let create position velocity radius colour =
    { Coord = position
      Velocity = velocity
      Radius = radius
      Color = colour }

  let randomParticle() =
    let randomIndex = (float palette.Length * JS.Math.random()) |> int
    { Coord =
        { X = JS.Math.random() * Width * 0.9 + (Width * 0.05)
          Y = JS.Math.random() * Height * 0.9 + (Height * 0.05) }
      Velocity =
        { X = JS.Math.random() * 2. - 1.
          Y = JS.Math.random() * 2. - 1. }
      Radius = 5.
      Color = palette.[randomIndex] }

  let getBounds particle =
    let vrad =
      { X = particle.Radius
        Y = particle.Radius }
    (particle.Coord - vrad, particle.Coord + vrad)

  let transformToScreenSpace vec =
    vec - { X = 0.
            Y = Height }
    |> Vector2.flipY

  let draw (ctx: CanvasRenderingContext2D) p =
    let ssp = transformToScreenSpace p.Coord
    ctx.beginPath()
    ctx.arc (ssp.X, ssp.Y, p.Radius, 0., 2. * System.Math.PI, false)
    ctx.fillStyle <- !^p.Color
    ctx.lineWidth <- 0.
    ctx.fill()
