namespace FableBounce

module App =

  open Fable.Core
  open Fable.Core.JsInterop
  open Browser.Types
  open Browser

  open Config
  open Particle

  let canvas = document.querySelector ("canvas") :?> HTMLCanvasElement
  let ctx = canvas.getContext_2d()

  canvas.width <- Width
  canvas.height <- Height

  let drawBg (ctx: CanvasRenderingContext2D) (canvas: HTMLCanvasElement) =
    ctx.fillStyle <- !^"#222"
    ctx.fillRect (0., 0., canvas.width, canvas.height)

  let drawText (text, x, y, (align, baseline)) =
    ctx.fillStyle <- !^"#999"
    ctx.font <- "Arial 40px"
    ctx.textAlign <- align
    ctx.textBaseline <- baseline

    ctx.fillText (text, x, y)

  let applyGravity delta p =
    if p.Coord.Y + p.Radius > 0.
    then { p with Velocity = { p.Velocity with Y = p.Velocity.Y - 0.001 * delta } }
    else p

  let move _delta p =
    let (|Move|Bounce|) p =
      if p.Coord.Y - p.Radius <= 0. && p.Velocity.Y < 0. then Bounce else Move

    let coord, velocity =
      p
      |> function
      | Bounce ->
          console.log ("BOUNCE")
          let v' = p.Velocity / 2. |> Vector2.flipY
          p.Coord + v', v'
      | Move -> p.Coord + p.Velocity |> Vector2.max { p.Coord with Y = p.Radius }, p.Velocity

    { p with
        Coord = coord
        Velocity = velocity }

  let seconds() = System.DateTime.Now.TimeOfDay.TotalSeconds
  let requestAnimationFrame = window.requestAnimationFrame >> ignore

  type State =
    | Spawning
    | Regular
    | Completed

  type Game =
    { StartSeconds: float
      State: State
      Particles: Particle list
      QuadTree: QuadTree<Particle> }

  module Game =
    let spawnParticle game =
      if floor (JS.Math.random() * 2.) = 0. then
        let particle = randomParticle()
        { game with Particles = particle :: game.Particles }
      else
        game

    let simulateParticles delta game =
      let particles = game.Particles |> List.map (applyGravity delta >> move delta)
      let quadTree =
        Rectangle.fromTLWH (0., 0., Width, Height) |> QuadTree.build particles Particle.getBounds
      { game with
          Particles = particles
          QuadTree = quadTree }

  let newGame() =
    let quadTree = Rectangle.fromTLWH (0., 0., Width, Height) |> QuadTree.createEmptyRoot
    console.log (quadTree)

    { StartSeconds = seconds()
      State = Spawning
      Particles = []
      QuadTree = quadTree }

  // The game loop
  // ========================


  let render delta elapsed game =
    drawBg ctx canvas
    for particle in game.Particles do
      draw ctx particle

    QuadTree.draw ctx game.QuadTree
    let message =
      sprintf "Delta: %f; Elapsed: %f; Particles count: %d" delta elapsed
        (game.Particles |> List.length)
    drawText (message, 20., 20., ("left", "top"))

    if (game.State = Completed) then
      drawText ("COMPLETED", Width / 2., Height / 2., ("center", "middle"))


  let update (delta: float) (elapsed: float) game =
    match game.State, elapsed with
    | Spawning, x when x < 4. ->
        game
        |> Game.spawnParticle
        |> Game.simulateParticles delta
    | Spawning, _ ->
        game
        |> Game.simulateParticles delta
        |> (fun g -> { g with State = Regular })
    | Regular, x when x < 15. -> game |> Game.simulateParticles delta
    | Regular, _ ->
        console.log (game.QuadTree)
        { game with State = Completed }
    | Completed, x when x < 1500. -> game
    | Completed, _ -> newGame()

  let rec run prevTimestamp game timestamp =
    let delta = timestamp - prevTimestamp
    let elapsed = seconds() - game.StartSeconds
    let newGame = update delta elapsed game

    render delta elapsed newGame
    run timestamp newGame |> requestAnimationFrame

  newGame()
  |> run 0.
  |> requestAnimationFrame
