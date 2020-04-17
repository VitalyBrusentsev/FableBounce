module Particles

open Fable.Import
open Fable.Core.JsInterop

let width = 900.
let height = 630.

let canvas = Browser.document.getElementsByTagName("canvas").[0] :?> Browser.HTMLCanvasElement
let ctx = canvas.getContext_2d()
canvas.width <- width
canvas.height <- height

let drawBg (ctx: Browser.CanvasRenderingContext2D) (canvas: Browser.HTMLCanvasElement) =
    ctx.fillStyle <- !^ "#777"
    ctx.fillRect ( 0.,0., canvas.width, canvas.height )

let drawText (text, x, y) =
    ctx.fillStyle <- !^ "#333"
    ctx.font <- "bold 40pt";
    ctx.fillText(text, x, y)

type Point = { X: float; Y: float }

type Particle =
    { Coord: Point; Velocity: Point; Radius:float; Color:string }

let palette = [| "red"; "green"; "orange"; "gray"; "teal" |]

let randomParticle () =
    let randomIndex = (float palette.Length * JS.Math.random()) |> int
    {  Coord = { X = JS.Math.random() * width*0.8 + (width*0.1); Y = 600. } 
       Velocity = { X = JS.Math.random() * 2. - 1.; Y = 0.0 }
       Radius = 10.
       Color = palette.[randomIndex] }

let drawParticle 
    (ctx: Browser.CanvasRenderingContext2D)
    (canvas: Browser.HTMLCanvasElement) p =
    ctx.beginPath()
    ctx.arc
        ( p.Coord.X, canvas.height - (p.Coord.Y + p.Radius),
          p.Radius, 0., 2. * System.Math.PI, false )
    ctx.fillStyle <- !^ p.Color
    ctx.fill()
    ctx.lineWidth <- 3.
    ctx.strokeStyle <- !^ p.Color
    ctx.stroke()


let applyGravity delta p =
    if p.Coord.Y > 0. then
        { p with Velocity = {p.Velocity with Y = p.Velocity.Y - 0.005 * delta } }
    else p

let move delta p =
    let (|Move|Bounce|) p = 
        if p.Coord.Y = 0. && p.Velocity.Y < 0. then Bounce else Move

    let coord, velocity = p |> function
        | Bounce -> 
            { X = p.Coord.X + p.Velocity.X
              Y = (p.Coord.Y + p.Velocity.Y) }, 
            { X = p.Velocity.X / 2.; Y = - p.Velocity.Y / 2. }
        | Move -> 
            { X = p.Coord.X + p.Velocity.X
              Y = max 0. (p.Coord.Y + p.Velocity.Y) }, p.Velocity
    { p with Coord = coord; Velocity = velocity }

let addParticles particles =
    if floor(JS.Math.random()*8.) = 0. then
        let particle = randomParticle()
        particle::particles
    else particles

let seconds() = System.DateTime.Now.TimeOfDay.TotalSeconds
let requestAnimationFrame = Browser.window.requestAnimationFrame >> ignore

type State = Spawning | Regular | Completed

type Game = 
    {   StartSeconds: float
        State: State
        Particles: Particle list }

let newGame() = { StartSeconds = seconds(); State = Spawning; Particles = [] }
    
// The game loop
// ========================

let recalc delta particles  = 
    particles 
    |> List.map (applyGravity delta)
    |> List.map (move delta)

let render delta elapsed game =
    drawBg ctx canvas
    for particle in game.Particles do drawParticle ctx canvas particle
    let message = 
        sprintf "Delta: %f; Elapsed: %f; Particles count: %d" 
            delta elapsed (game.Particles |> List.length)
    drawText (message, 20., 20.)
    if (game.State = Completed) then
        drawText ("COMPLETED", 320., 300.)


let update (delta: float) (elapsed: float) game = 
    match game.State, elapsed with
        | Spawning, x when x < 2. -> 
            { game with Particles = game.Particles |> addParticles |> recalc delta }
        | Spawning, _ -> 
            { game with State = Regular; Particles = game.Particles |> recalc delta }
        | Regular, x when x < 7. -> 
            { game with Particles = game.Particles |> recalc delta }
        | Regular, _ -> 
            { game with State = Completed }
        | Completed, x when x < 15. -> game
        | Completed, _ -> newGame()

let rec gameFrame prevTimestamp game  timestamp = 
    let delta = timestamp - prevTimestamp
    let elapsed = seconds() - game.StartSeconds
    let newGame = update delta elapsed game
    render delta elapsed newGame
    gameFrame timestamp newGame |> requestAnimationFrame

newGame() 
|> gameFrame 0.
|> requestAnimationFrame