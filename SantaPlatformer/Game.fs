module MakeSantaJump
     
open System.Collections.Generic
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open Microsoft.FSharp.Collections

//These values change the gameplay
let mutable gravity = 0.0325f
let mutable speed = -0.3f
let minObstacleWidth = 20
let maxObstacleWidth = 35
let minObstacleHeight = 15
let maxObstacleHeight = 40
let mutable avoidedObstacles = 0
let highScoreList = new ResizeArray<int>()
highScoreList.Add(0)
let mutable dubJump = false

type SpriteTexture =
    {
        texture : Texture2D;
        textureData : Color array;
        spriteWidth : int;
        numSprites : int;
    }

type Santa (spriteTexture : SpriteTexture, trigger, startBottom) = 
    let santaX = 50
    let santaWidth = spriteTexture.spriteWidth
    let santaHeight = spriteTexture.texture.Height

    let mutable y = single (startBottom-santaHeight)
    let mutable dy = 0.0f
    let mutable isJumping = false
    let mutable spriteTimer = 0.0f
    let mutable spriteIndex = 0

    member this.Bounds 
        with get() = Rectangle(santaX, int(y), santaWidth, santaHeight) 
    
    member this.Update(deltaTime, isKeyPressedSinceLastFrame : Keys -> bool, trackBounds : Rectangle) = 
        if not dubJump then 
            if not isJumping && isKeyPressedSinceLastFrame(trigger) then
                isJumping <- true
                dy <- -0.5f
        else 
            if isKeyPressedSinceLastFrame(trigger) then
                isJumping <- true
                dy <- -0.5f 
        
        if isJumping then 
            y <- y + dy * deltaTime

            let hasLanded = int(y) + santaHeight >= trackBounds.Bottom
            if hasLanded then
                y <- single(trackBounds.Bottom - santaHeight)
                isJumping <- false
                dy <- 0.0f
            else 
                dy <- dy + gravity
        
        else
            let spriteChangeTime = 80.0f
            spriteTimer <- spriteTimer + deltaTime
            if spriteTimer >= spriteChangeTime then 
                spriteTimer <- 0.0f
                let wrap value max = 
                    if value > max then 0 else value
                spriteIndex <- wrap (spriteIndex+1) (spriteTexture.numSprites - 1)
   
    member this.Draw(spriteBatch : SpriteBatch) = 
        let spriteBounds =
            Rectangle(spriteIndex * spriteTexture.spriteWidth, 0, spriteTexture.spriteWidth, spriteTexture.texture.Height)
        spriteBatch.Draw(spriteTexture.texture, this.Bounds, System.Nullable(spriteBounds), Color.White)
    
    member this.AnyNonTransparentPixels(x1, x2, y1, y2) =
        let xOffset = spriteTexture.spriteWidth * spriteIndex
        let pixelsInsideRegion = seq {
            for y in y1..y2-1 do
                for x in (x1 + xOffset)..(x2 + xOffset - 1) do
                    let index = (y * spriteTexture.texture.Width) + x
                    yield spriteTexture.textureData.[index]
        }
        Seq.exists (fun c -> c <> Color.Transparent) pixelsInsideRegion

type speedUp (startX, width, height) =
    let mutable x = startX
    member this.Visible 
        with get() = int(x) + width > 0 
    member this.GetBounds(trackBounds : Rectangle) = 
        Rectangle(int(x), trackBounds.Bottom - 100, width, height)
    member this.Update(deltaTime) = 
        x <- x + speed * deltaTime
    member this.Draw(spriteBatch : SpriteBatch, texture, trackBounds : Rectangle) =
        spriteBatch.Draw(texture, this.GetBounds(trackBounds), Color.Black)    
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]

module speedUp = 
    let rng = System.Random(200)
    let addNewBoost (trackBounds : Rectangle) (speedUps : speedUp list) = 
        let isMostRecentlyAddedBoostFullyVisible = 
            match speedUps with 
            | head :: tail -> head.GetBounds(trackBounds).Right < trackBounds.Right
            | [] -> true
        
        //let isSpeedNorm =    //causes too much lag
            //if speed <= -0.03f then
                //true
            //else 
                //false

        if isMostRecentlyAddedBoostFullyVisible then
            if avoidedObstacles>8 then 
                let x = trackBounds.Right + 1600 + rng.Next(500)
                let width = rng.Next(20, 35)
                let height = rng.Next(10, 15)
                let newSpeedUp = speedUp(single(x), width, height)
                newSpeedUp :: speedUps
            else speedUps
        else
            speedUps
    
    let removeOldBoosts (speedUps : speedUp list) = 
        speedUps |> List.filter(fun s -> s.Visible)

type JumpBig (startX, width, height) = 
    let mutable x = startX
    member this.Visible
        with get() = int(x) + width > 0 
    member this.GetBounds(trackBounds : Rectangle) = 
        Rectangle(int(x), trackBounds.Bottom - 100, width, height)
    member this.Update(deltaTime) = 
        x <- x + speed * deltaTime
    member this.Draw(spriteBatch : SpriteBatch, texture, trackBounds : Rectangle) =
        spriteBatch.Draw(texture, this.GetBounds(trackBounds), Color.DarkViolet)
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]    

module JumpBig = 
    let rng = System.Random(200)
    let addNewJumpBig (trackBounds : Rectangle) (jumpBigs : JumpBig list) =
        let isMostRecentJumpBigFullyVisible = 
            match jumpBigs with 
            | head :: tail -> head.GetBounds(trackBounds).Right < trackBounds.Right
            | [] -> true
        
        let JumpBigIsOff = 
            if gravity >= 0.03f then
                true
            else 
                false
        
        if isMostRecentJumpBigFullyVisible then
            if JumpBigIsOff && avoidedObstacles> 4 then
                let x = trackBounds.Right + 1700 + rng.Next(500)   
                let width = rng.Next(10, 15)
                let height = rng.Next(20, 30)
                let newJumpBig = JumpBig(single(x), width, height)
                newJumpBig :: jumpBigs
            else jumpBigs
        else
            jumpBigs
    
    let removeOldJumpBigs (jumpBigs : JumpBig list) = 
        jumpBigs |> List.filter(fun j -> j.Visible)

type Boost (startX, width, height) = 
    let mutable x = startX
    member this.Visible
        with get() = int(x) + width > 0 
    member this.GetBounds(trackBounds : Rectangle) = 
        Rectangle(int(x), trackBounds.Bottom - 100, width, height)
    member this.Update(deltaTime) = 
        x <- x + speed * deltaTime
    member this.Draw(spriteBatch : SpriteBatch, texture, trackBounds : Rectangle) =
        spriteBatch.Draw(texture, this.GetBounds(trackBounds), Color.LightSalmon)
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]

module Boost =
    let rng = System.Random(200)
    let addNewBoost (trackBounds : Rectangle) (boosts : Boost list) = 
        let isMostRecentlyAddedBoostFullyVisible = 
            match boosts with 
            | head :: tail -> head.GetBounds(trackBounds).Right < trackBounds.Right
            | [] -> true

        if isMostRecentlyAddedBoostFullyVisible then
            if avoidedObstacles> 2 then
                let x = trackBounds.Right + 1400 + rng.Next(500)  
                let width = rng.Next(10, 15)
                let height = rng.Next(20, 30)
                let newBoost = Boost(single(x), width, height)
                newBoost :: boosts
            else boosts
        else
            boosts
    
    let removeOldBoosts (boosts : Boost list) = 
        boosts |> List.filter(fun b -> b.Visible)


type Obstacle (startX, width, height) = 
    let mutable x = startX

    member this.Visible
        with get() = int(x) + width > 0
    
    member this.GetBounds(trackBounds : Rectangle) = 
        Rectangle(int(x), trackBounds.Bottom - height, width, height)
    
    member this.Update(deltaTime) = 
        x <- x + speed * deltaTime
    
    member this.Draw(spriteBatch : SpriteBatch, texture, trackBounds : Rectangle) =
        spriteBatch.Draw(texture, this.GetBounds(trackBounds), Color.Green)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Obstacle = 
    let rng = System.Random()

    let addNewObstacles (trackBounds : Rectangle) (obstacles : Obstacle list) = 
        let isMostRecentlyAddedObstacleFullyVisible = 
            match obstacles with 
            | head :: tail -> head.GetBounds(trackBounds).Right < trackBounds.Right
            | [] -> true
        
        if isMostRecentlyAddedObstacleFullyVisible then 
            let x = trackBounds.Right + 200 + rng.Next(200)
            let width = rng.Next(minObstacleWidth, maxObstacleWidth)
            //let height = 15
            let height = rng.Next(minObstacleHeight, maxObstacleHeight)
            let newObstacle = Obstacle(single(x), width, height)
            newObstacle :: obstacles
        else
            obstacles
    
    let removeOldObstacles (obstacles : Obstacle list) = 
        obstacles |> List.filter(fun o -> o.Visible)


type Track(color, bounds : Rectangle, spriteTexture, triggerKey) = 
    let mutable obstacles = List.empty<Obstacle>
    let mutable boosts = List.empty<Boost>
    let mutable speedUps = List.empty<speedUp>
    let mutable jumpBigs = List.empty<JumpBig>
    let santa = Santa(spriteTexture, triggerKey, bounds.Bottom)
    member this.AvoidedObstacles
        with get() = avoidedObstacles

    member this.Update(deltaTime, isKeyPressedSinceLastFrame) =
        santa.Update(deltaTime, isKeyPressedSinceLastFrame, bounds)
        for obstacle in obstacles do
            obstacle.Update(deltaTime)
        for boost in boosts do
            boost.Update(deltaTime)
        for jumpBig in jumpBigs do 
            jumpBig.Update(deltaTime)
        for speedUp in speedUps do
            speedUp.Update(deltaTime)
        
        let oldObstaclesCount = 
            obstacles
            |> List.filter (fun o -> not o.Visible)
            |> List.length
        avoidedObstacles <- avoidedObstacles + oldObstaclesCount

        obstacles <- obstacles
            |> Obstacle.removeOldObstacles
            |> Obstacle.addNewObstacles bounds
        boosts <- boosts
            |> Boost.removeOldBoosts
            |> Boost.addNewBoost bounds 
        jumpBigs <- jumpBigs
            |> JumpBig.removeOldJumpBigs
            |> JumpBig.addNewJumpBig bounds 
        speedUps <- speedUps
            |> speedUp.removeOldBoosts
            |> speedUp.addNewBoost bounds

    member this.Draw(spriteBatch : SpriteBatch, texture, fontRenderer : FontRendering.FontRenderer) =
        spriteBatch.Draw(texture, bounds, color) // Track background
        for obstacle in obstacles do
            obstacle.Draw(spriteBatch, texture, bounds)
        for boost in boosts do 
            boost.Draw(spriteBatch, texture, bounds)
        for jumpBig in jumpBigs do 
            jumpBig.Draw(spriteBatch, texture, bounds)
        for speedUp in speedUps do 
            speedUp.Draw(spriteBatch, texture, bounds)
        santa.Draw(spriteBatch)
        fontRenderer.DrawText(spriteBatch, 10, 10 + bounds.Y, triggerKey.ToString()) 
    
    member this.HitBoost() = 
        let santaBounds = santa.Bounds
        let boostCollidingWithSanta (boost: Boost) = 
            let boostBounds = boost.GetBounds(bounds)

            if santaBounds.Intersects(boostBounds) then
               
                let x1 = max (boostBounds.X - santaBounds.X) 0
                let x2 = min (boostBounds.Right - santaBounds.X) santaBounds.Width

                let y1 = max (boostBounds.Y - santaBounds.Y) 0
                let y2 = min (boostBounds.Bottom - santaBounds.Y) santaBounds.Height
       
                santa.AnyNonTransparentPixels(x1,x2, y1, y2)    
            else
                false
        List.exists boostCollidingWithSanta boosts

    member this.HitJumpBig() = 
        let santaBounds = santa.Bounds
        let JumpBigCollidingWithSanta (jumpBig: JumpBig) = 
            let jumpBigBounds = jumpBig.GetBounds(bounds)
            if santaBounds.Intersects(jumpBigBounds) then
               
                let x1 = max (jumpBigBounds.X - santaBounds.X) 0
                let x2 = min (jumpBigBounds.Right - santaBounds.X) santaBounds.Width

                let y1 = max (jumpBigBounds.Y - santaBounds.Y) 0
                let y2 = min (jumpBigBounds.Bottom - santaBounds.Y) santaBounds.Height
       
                santa.AnyNonTransparentPixels(x1,x2, y1, y2)    
            else
                false
        List.exists JumpBigCollidingWithSanta jumpBigs
    member this.HitSpeedUp() = 
        let santaBounds = santa.Bounds
        let speedUpCollidingWithSanta (speedUp: speedUp) = 
            let speedUpBounds = speedUp.GetBounds(bounds)
            if santaBounds.Intersects(speedUpBounds) then
               
                let x1 = max (speedUpBounds.X - santaBounds.X) 0
                let x2 = min (speedUpBounds.Right - santaBounds.X) santaBounds.Width

                let y1 = max (speedUpBounds.Y - santaBounds.Y) 0
                let y2 = min (speedUpBounds.Bottom - santaBounds.Y) santaBounds.Height
                
                santa.AnyNonTransparentPixels(x1,x2, y1, y2)
                
            else
                false
        List.exists speedUpCollidingWithSanta speedUps
    
    member this.HasCollisions() =
        let santaBounds = santa.Bounds

        let obstacleCollidingWithSanta (obstacle : Obstacle) =
            // First do simple intersection.
            let obstacleBounds = obstacle.GetBounds(bounds)

            if santaBounds.Intersects(obstacleBounds) then
                // If the bounding rectangles overlap, then do pixel-perfect collision detection.
                let x1 = max (obstacleBounds.X - santaBounds.X) 0
                let x2 = min (obstacleBounds.Right - santaBounds.X) santaBounds.Width

                let y1 = max (obstacleBounds.Y - santaBounds.Y) 0
                let y2 = min (obstacleBounds.Bottom - santaBounds.Y) santaBounds.Height

                santa.AnyNonTransparentPixels(x1, x2, y1, y2)
            else
                false
        List.exists obstacleCollidingWithSanta obstacles
   
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Track =
    let createTracks (gameBounds : Rectangle) spriteTexture numTracks =
        let padding = 10
        let totalPadding = (numTracks - 1) * padding
        let availableHeight = gameBounds.Height - totalPadding
        let trackHeight = availableHeight / numTracks

        let colors = [ Color.SkyBlue; Color.Gold; Color.Lavender; Color.Brown; Color.Gold ]
        let keys = [ Keys.A; Keys.S; Keys.D; Keys.F; Keys.Space ]

        let makeTrack i =
            let trackBounds = Rectangle(0, i * (trackHeight + padding), gameBounds.Width, trackHeight)
            Track(colors.[i], trackBounds, spriteTexture, keys.[i + (keys.Length - numTracks)])

        List.init numTracks makeTrack

type GameState =
    | Welcome
    | MainMenu
    | Game
    | GamePaused
    | GameOver

type MakeSantaJumpGame() as this = 
    inherit Game()
    do this.Window.Title <- "Make Santa Jump"
    
    let graphics = new GraphicsDeviceManager(this)
    do graphics.PreferredBackBufferWidth <- 800
    do graphics.PreferredBackBufferHeight <- 600

    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>
    let mutable texture = Unchecked.defaultof<Texture2D>
    let mutable spriteTexture = Unchecked.defaultof<SpriteTexture>
    let mutable fontRenderer = Unchecked.defaultof<FontRendering.FontRenderer>
    let mutable gameState = Welcome
    let mutable tracks = []
    let mutable lastKeyState = KeyboardState()
    let mutable boostNum = 0
    let mutable speedUpNum = 0
      

    override this.LoadContent() =
       
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)

        texture <- new Texture2D(this.GraphicsDevice, 1, 1)
        texture.SetData([| Color.White |])
        let mutable rand = System.Random()
        if rand.NextDouble()<0.5 then 
            use santaStream = System.IO.File.OpenRead("Santa.png")
            let santaTexture = Texture2D.FromStream(this.GraphicsDevice, santaStream)
            let santaTextureData = Array.create<Color> (santaTexture.Width * santaTexture.Height) Color.Transparent
            santaTexture.GetData(santaTextureData)
            spriteTexture <- { texture = santaTexture;
                           textureData = santaTextureData;
                           spriteWidth = santaTexture.Width / 8;
                           numSprites = 8 }
        else 
            use santaStream = System.IO.File.OpenRead("HorseSprite2.png")
            let santaTexture = Texture2D.FromStream(this.GraphicsDevice, santaStream)
            let santaTextureData = Array.create<Color> (santaTexture.Width * santaTexture.Height) Color.Transparent
            santaTexture.GetData(santaTextureData)
            spriteTexture <- { texture = santaTexture;
                           textureData = santaTextureData;
                           spriteWidth = santaTexture.Width / 4;
                           numSprites = 4 }
        
        use fontTextureStream = System.IO.File.OpenRead("GameFont_0.png")
        let fontTexture = Texture2D.FromStream(this.GraphicsDevice, fontTextureStream)
        let fontFile = FontRendering.FontLoader.Load("GameFont.fnt")
        fontRenderer <- FontRendering.FontRenderer(fontFile, fontTexture)
        //tracks <- [ Track(Color.SkyBlue, this.GraphicsDevice.Viewport.Bounds, spriteTexture, Keys.Space) ]
        tracks <- Track.createTracks this.GraphicsDevice.Viewport.Bounds spriteTexture 5
        //decide how many players for the game

    override this.Update(gameTime) =
        let currentKeyState = Keyboard.GetState()
        let deltaTime = single(gameTime.ElapsedGameTime.TotalMilliseconds)
        let avoidedObstacles2 = List.sumBy (fun (o : Track) -> o.AvoidedObstacles) tracks 
        let isKeyPressedSinceLastFrame key =
            currentKeyState.IsKeyDown(key) && lastKeyState.IsKeyUp(key)

        match gameState with
        |Welcome ->
            if isKeyPressedSinceLastFrame Keys.Space then gameState <- MainMenu
        | MainMenu ->
            avoidedObstacles <- 0
            let startGame numTracks =
                tracks <- Track.createTracks this.GraphicsDevice.Viewport.Bounds spriteTexture numTracks
                gameState <- Game

            if isKeyPressedSinceLastFrame Keys.D1 then startGame 1
            elif isKeyPressedSinceLastFrame Keys.D2 then startGame 2
            elif isKeyPressedSinceLastFrame Keys.D3 then startGame 3
            //elif isKeyPressedSinceLastFrame Keys.D4 then startGame 4
            //elif isKeyPressedSinceLastFrame Keys.D5 then startGame 5
            
        | Game ->
            if isKeyPressedSinceLastFrame Keys.P then
                gameState <- GamePaused
            else 
                for track in tracks do
                    track.Update(deltaTime, isKeyPressedSinceLastFrame)
                if List.exists (fun (t : Track) -> t.HitBoost()) tracks then
                    boostNum <- avoidedObstacles     
                    dubJump <- true
                    //gravity <- 0.02f
                if avoidedObstacles> boostNum+5 then  
                    dubJump <- false
                    gravity <- 0.0325f
                if List.exists (fun (t: Track) -> t.HitJumpBig()) tracks then 
                    gravity <- 0.02f
                if List.exists (fun (t: Track) -> t.HitSpeedUp()) tracks then
                    speed <- -0.35f
                if List.exists (fun (t : Track) -> t.HasCollisions()) tracks then
                    gameState <- GameOver
        | GamePaused ->
            if isKeyPressedSinceLastFrame Keys.P then
                gameState <- Game
        | GameOver ->
            if avoidedObstacles2 > highScoreList.[0] then 
                highScoreList.Insert(0, avoidedObstacles2)
            gravity <- 0.0325f
            speed <- -0.3f
            dubJump <- false
            if isKeyPressedSinceLastFrame Keys.Space then
                gameState <- MainMenu

        lastKeyState <- currentKeyState
   
    override this.Draw(gameTime) =
        this.GraphicsDevice.Clear Color.DarkSeaGreen
        spriteBatch.Begin(SpriteSortMode.Deferred, BlendState.NonPremultiplied)
        let avoidedObstacles = List.sumBy (fun (o : Track) -> o.AvoidedObstacles) tracks 
        
        match gameState with
        | Welcome ->
            fontRenderer.DrawText(spriteBatch, 200, 200, "Welcome to Realty Run")
            fontRenderer.DrawText(spriteBatch, 125, 300, "Click the space bar to continue")
        | MainMenu ->
            fontRenderer.DrawText(spriteBatch, 490, 50, "High Score:")
            fontRenderer.DrawText(spriteBatch, 700, 50, highScoreList.[0].ToString())
            fontRenderer.DrawText(spriteBatch, 100, 50, "Choose a difficulty.")
            fontRenderer.DrawText(spriteBatch, 150, 200, "1 - Normal")
            fontRenderer.DrawText(spriteBatch, 150, 250, "2 - All-Star")
            fontRenderer.DrawText(spriteBatch, 150, 300, "3 - Legend")
            //fontRenderer.DrawText(spriteBatch, 100, 300, "4 - Very Hard")
            //fontRenderer.DrawText(spriteBatch, 100, 350, "5 - Impossible")
        | Game
        | GamePaused ->
            for track in tracks do
                track.Draw(spriteBatch, texture, fontRenderer)
            fontRenderer.DrawText(spriteBatch, this.GraphicsDevice.Viewport.Bounds.Right - 60, 30,
                                  avoidedObstacles.ToString())
        | GameOver ->
            for track in tracks do
                track.Draw(spriteBatch, texture, fontRenderer)
            fontRenderer.DrawText(spriteBatch, 100, 100, "Game Over!")
            fontRenderer.DrawText(spriteBatch, 100, 150, "Press Space to continue.")
            fontRenderer.DrawText(spriteBatch, this.GraphicsDevice.Viewport.Bounds.Right - 60, 30,
                                  avoidedObstacles.ToString())

        spriteBatch.End()



