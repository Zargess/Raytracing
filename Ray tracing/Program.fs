
// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System
open System.Drawing
open System.Windows.Forms
open System.Windows.Media.Media3D

type Color(r: float, g: float, b: float) = 
    member this.r = r
    member this.g = g
    member this.b = b
    static member ( * ) (c1: Color, c2: Color) =
        Color (c1.r * c2.r, c1.g * c2.g, c1.b * c2.b)
    static member ( * ) (c: Color, s: float) = 
        Color (c.r * s, c.g * s, c.b * s)
    static member ( + ) (c1: Color, c2: Color) = 
        let r = c1.r + c2.r
        let g = c1.g + c2.g
        let b = c1.b + c2.b
        Color(r,g,b)
    static member Zero = Color(0.0, 0.0, 0.0)
     member this.clip = 
        let r1 = min 1.0 r
        let g1 = min 1.0 g
        let b1 = min 1.0 b
        Color(r1,g1,b1)

type Shape =
    | Sphere  of center:Vector3D * radius:float * diffuseColor:Color
    | Triangle of a:Vector3D * b:Vector3D * c:Vector3D * v1:Vector3D * v2:Vector3D * v3:Vector3D * diffuseColor:Color
type Light = { shape:Shape; color:Color }
type Camera = { position:Vector3D; lookAt:Vector3D; lookUp:Vector3D }
type Scene = { camera:Camera; shapes:list<Shape>; ambientLight:Color; lights:list<Light> }
type Ray = { origin:Vector3D; direction:Vector3D }
type Intersection = { normal:Vector3D; point:Vector3D; ray:Ray; shape:Shape; t:float; color:Color }
type LightSample = { point:Vector3D; color:Color; normal:Vector3D }
type LightInterval = { light:Light; first:float; last:float }
   

let norm (v:Vector3D) =
    let abs = sqrt (v.X * v.X + v.Y * v.Y + v.Z * v.Z)
    v / abs

let cramersDet v1 v2 v3 =
    Vector3D.DotProduct(Vector3D.CrossProduct(v1, v2), v3)

let cramers a b c ray =
    let A1 = a - b
    let A2 = a - c
    let A3 = ray.direction
    let A4 = a - ray.origin
    let detA = cramersDet A1 A2 A3
    let det1 = cramersDet A4 A2 A3
    let det2 = cramersDet A1 A4 A3
    let det3 = cramersDet A1 A2 A4
    let beta = det1 / detA
    let y = det2 /detA
    let t = det3 / detA
    let alpha = 1.0 - beta - y
    (alpha,beta,y,t)

let pointAtTime ray time =
    ray.origin + time * ray.direction

let pointOnSphere (rand:Random)=
    let phi = rand.NextDouble() * 2.0 * Math.PI
    let theta = rand.NextDouble() * Math.PI
    Vector3D((Math.Cos phi) * (Math.Sin theta), (Math.Sin phi) * (Math.Cos theta), Math.Cos theta)

let getSampleFromShape shape (rand:Random) = 
    match shape with
        | Sphere (center, radius, diffuseColor) ->
            let randPoint = pointOnSphere rand
            let point = radius * randPoint + center;
            { point = point; color = diffuseColor; normal=randPoint }
        | Triangle(a,b,c,v1,v2,v3,color) ->
            let alpha = rand.NextDouble()
            let beta = rand.NextDouble()
            let y = 1.0 - alpha - beta
            let point = alpha * a + beta * b + y * c
            { point = point; color = color; normal=alpha * v1 + beta * v2 + y * v3 }


let intersection shape ray = 
    match shape with
        | Sphere (center, radius, diffuseColor) ->
            let s = ray.origin - center
            let rayDir = norm ray.direction
            let sv = Vector3D.DotProduct(s, rayDir)
            let ss = Vector3D.DotProduct(s,s)
            let discr = sv*sv - ss + radius * radius
            if discr < 0.0 then []
            else
                let normalAtTime t = norm ((pointAtTime ray t) - center)
                let (t1,t2) = (-sv + sqrt(discr), -sv - sqrt(discr))
                [ { normal = normalAtTime t1; point = pointAtTime ray t1; ray = ray; shape=shape; t = t1; color=diffuseColor };
                  { normal = normalAtTime t2; point = pointAtTime ray t2; ray = ray; shape=shape; t = t2; color=diffuseColor } ]
        | Triangle(a,b,c,v1,v2,v3,color) ->
            let (alpha,beta,y,t) = cramers a b c ray
            let n = alpha * v1 + beta * v2 + y * v3
            let p = pointAtTime ray t
            [ { normal = n; point = p; ray = ray; shape = shape; t = t; color=color } ]

let raytrace ray (scene : Scene) min max =
    List.filter (fun x -> x.t > min && x.t < max) (List.collect (fun x -> x) (List.map (fun x -> intersection x ray) scene.shapes))

let getArea shape =
    match shape with
        | Sphere(center, radius, diffuseColor) ->
            4.0 * Math.PI * radius * radius
        | Triangle(a,b,c,v1,v2,v3,color) ->
            Vector3D.CrossProduct(v1, v2).Length / 2.0
// lav recursiv
let computeLightIntervals (lights : list<Light>) = 
    let mutable lastEnd = 0.0
    let areas = seq {
        for light in lights do
            yield getArea light.shape
    }
    let areaTot = Seq.sum areas
    let intervals = seq {
        for light in lights do
            let area = getArea light.shape
            let percent = (area / areaTot) * 100.0
            let first = lastEnd + 1.0
            let last = lastEnd + percent
            lastEnd <- last
            yield { light = light; first = first; last = last }
    }
    intervals


let getLight lightintervals (rand : Random) =
    let random = rand.NextDouble() * 100.0
    Seq.find (fun x -> x.first <= random && x.last >= random) lightintervals

let colorAt intersections (scene : Scene) rand (lightintervals : seq<LightInterval>) =
    let closest = List.minBy(fun i -> i.t) intersections
    let light = (getLight lightintervals rand).light
    let sample = getSampleFromShape light.shape rand
    let toLight = sample.point - closest.point
    let L = norm toLight
    let tmin = 0.01
    let tmax = toLight.Length
    let shadowray = { origin = closest.point; direction = L }
    if (raytrace shadowray scene tmin tmax).Length > 0 then closest.color * scene.ambientLight
    else 
        let kd = closest.color
        let Ia = scene.ambientLight
        let Id = Math.Max(0.0, Vector3D.DotProduct(L, closest.normal))
        let V = scene.camera.position - closest.point
        let H = norm (L + V)
        let Is = Math.Pow(Math.Max(0.0, Vector3D.DotProduct(H,closest.normal)), 500.0)
        let ambient = kd * Ia
        let diffuse = kd * Id * sample.color
        let specular = Color(1.0,1.0,1.0) * Is
        ambient + diffuse + specular

type RenderForm(width : int, height : int, scene : Scene, rand : Random, vpc : Vector3D, pw : float, ph : float, u : Vector3D, v : Vector3D, intervals : seq<LightInterval>) as form =
        inherit Form()
        let mutable x = 0
        let mutable y = 0
        let mutable Done = false
        let bmp = new Bitmap(width, height)
        do 
            form.InitializeForm


        member this.InitializeForm =
            this.Text <- "FRay"
            this.Width <- 480
            this.Height <- 320

        override form.OnPaint e =
            let g = e.Graphics
            if not Done then
                let colors = seq { 
                      for i in 0..15 do
                        let dx = rand.NextDouble() - 0.5
                        let dy = rand.NextDouble() - 0.5
                        let rayPoint = vpc + (dx + float(x-form.Width/2))*pw*u + (dy + float(y-form.Height/2))*ph*v
                        let rayDir = norm (rayPoint - scene.camera.position)
                        let ray = { origin = scene.camera.position; direction = rayDir }
                        let intersects = raytrace ray scene 0.01 100.0
                        yield  match intersects with
                               | [] -> Color.Zero
                               | _ ->  colorAt intersects scene rand intervals                         
                     }
                let color = ((Seq.sum colors) * (1.0 / 16.0)).clip
                bmp.SetPixel(x,y, Color.FromArgb(255, (int)(color.r*255.0), (int)(color.g*255.0), (int)(color.b*255.0)))
            
                y <- y + 1
                if y = height then 
                    y <- 0
                    x <- x + 1
                    let img = bmp :> Image
                    g.DrawImage(img, new Point(0,0))

                if x = width then
                    Done <- true
                    bmp.Save(@"c:\users\mfh\desktop\output1.jpg")
                else
                    form.Invalidate()

do
    let width = 480
    let height = 320

    // Vertical and horiontal field of view:
    let hfov = Math.PI/3.5
    let vfov = hfov * float(height) / float(width)

    // Pixel width and height
    let pw = 2.0 * Math.Tan(float(hfov / 2.0)) / float(width)
    let ph = 2.0 * Math.Tan(float(vfov / 2.0)) / float(height)

    // Setting up the UI components
    let mainForm = new Form(Width = width, Height = height, Text = "FRay")
    let box = new PictureBox(BackColor = Color.White, Dock = DockStyle.Fill, SizeMode = PictureBoxSizeMode.CenterImage)
    mainForm.Controls.Add(box)
    let bmp = new Bitmap(width, height)

    // Sphere
    let sphere = Sphere(Vector3D(2.0, 1.0, 0.8), 0.2, Color(1.0, 0.1, 0.1))
    let sphere2 = Sphere(Vector3D(1.0, 1.0, 1.0), 0.4, Color(0.1, 1.0, 0.1))
    let sphere3 = Sphere(Vector3D(1.0, 1.0, 100.0), 98.0, Color(0.1, 1.0, 0.1))

    // Camera
    let camera = { position = Vector3D(-5.0, 0.0, 0.0); lookAt = Vector3D(1.0, 1.0, 1.0); lookUp = Vector3D(0.0, 1.0, 0.0) }

    // Scene
    let sun = Sphere(Vector3D(0.0,0.0,-5.0), 0.5, diffuseColor=Color(1.0,1.0,1.0))
    let light : Light = { shape=sun; color=Color(0.8,0.8,0.8) }
    let scene = { camera = camera; shapes = [sphere; sphere2; sphere3]; ambientLight = Color(0.2, 0.2, 0.2); lights = [light] }

    // Set up the coordinate system
    let n = norm (camera.position - camera.lookAt)
    let u = norm (Vector3D.CrossProduct(n, camera.lookUp))
    let v = norm (Vector3D.CrossProduct(n, u))
    let vpc = camera.position - n

    let rand = Random()

    let intervals = computeLightIntervals scene.lights
    //let mainForm = new RenderForm(width, height, scene, rand, vpc, pw, ph, u, v)
//    Application.Run(mainForm)
//    let box = new PictureBox(BackColor = Color.White, Dock = DockStyle.Fill, SizeMode = PictureBoxSizeMode.CenterImage)
//    mainForm.Controls.Add(box)
//    box.Image <- (bmp :> Image)

    for x in 0..(width - 1) do
        for y in 0..(height - 1) do
            let colors = seq { 
                            for i in 0..15 do
                                let dx = rand.NextDouble() - 0.5
                                let dy = rand.NextDouble() - 0.5
                                let rayPoint = vpc + (dx + float(x-width/2))*pw*u + (dy + float(y-height/2))*ph*v
                                let rayDir = norm (rayPoint - scene.camera.position)
                                let ray = { origin = scene.camera.position; direction = rayDir }
                                let intersects = raytrace ray scene 0.01 100.0
                                yield  match intersects with
                                    | [] -> Color.Zero
                                    | _ ->  colorAt intersects scene rand intervals                       
                              }
            let color = ((Seq.fold (fun a b -> a + b) Color.Zero colors) * (1.0 / 16.0)).clip
            bmp.SetPixel(x,y, Color.FromArgb(255, (int)(color.r*255.0), (int)(color.g*255.0), (int)(color.b*255.0)))
           
    box.Image <- (bmp :> Image)

    bmp.Save(@"c:\users\mfh\desktop\output1.jpg")
    Console.WriteLine("Done")
    Application.Run(mainForm)