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
    static member ( / ) (c: Color, s : float) =
        let r = c.r / s
        let g = c.g / s
        let b = c.b / s
        Color(r,g,b)
    static member Zero = Color(0.0, 0.0, 0.0)
     member this.clip = 
        let r1 = min 1.0 r
        let g1 = min 1.0 g
        let b1 = min 1.0 b
        Color(r1,g1,b1)

type Material = { reflectivity:float; diffusefactor:float; diffuseColor:Color; specularColor:Color }
type Shape =
    | Sphere  of center:Vector3D * radius:float * material:Material
    | Triangle of a:Vector3D * b:Vector3D * c:Vector3D * v1:Vector3D * v2:Vector3D * v3:Vector3D * material:Material
type Light = { shape:Shape; color:Color }
type Camera = { position:Vector3D; lookAt:Vector3D; lookUp:Vector3D }
type Scene = { camera:Camera; shapes:list<Shape>; ambientLight:Color; lights:list<Light> }
type Ray = { origin:Vector3D; direction:Vector3D }
type Intersection = { normal:Vector3D; point:Vector3D; ray:Ray; shape:Shape; t:float; material:Material }
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

let weightedDirection (rand:Random) = 
    let u1 = rand.NextDouble()
    let u2 = rand.NextDouble()
    let theta = Math.Acos(sqrt(u1))
    let phi = 2.0 * Math.PI * u2
    Vector3D((Math.Cos phi) * (Math.Sin theta), (Math.Sin phi) * (Math.Cos theta), Math.Cos theta)

let perpendicular (vec : Vector3D) =
    let ax = Math.Abs vec.X
    let ay = Math.Abs vec.Y
    let az = Math.Abs vec.Z

    let uyx = (ax-ay) < 0.0
    let uzx = (ax - az) < 0.0
    let uzy = (ay - az) < 0.0

    let xm = uyx && uzx
    let ym = (true<>xm) && uzy
    let zm = true<>(xm && ym)

    let tx = if xm then 1.0 else 0.0
    let ty = if ym then 1.0 else 0.0
    let tz = if zm then 1.0 else 0.0

    Vector3D.CrossProduct(vec, Vector3D(tx,ty,tz))

let generateWeightedDirection (rand:Random) (norm:Vector3D) =
    let vec = weightedDirection rand
    let u = perpendicular norm
    let v = Vector3D.CrossProduct(vec,u)
    let res = u*vec.X + v*vec.Y + vec.Z*norm
    let q = res.Normalize()
    res

let getSampleFromShape shape (rand:Random) = 
    match shape with
        | Sphere (center, radius, material) ->
            let randPoint = pointOnSphere rand
            let point = radius * randPoint + center;
            let color = material.diffuseColor
            { point = point; color = color; normal=randPoint }
        | Triangle(a,b,c,v1,v2,v3,material) ->
            let alpha = rand.NextDouble()
            let beta = rand.NextDouble()
            let y = 1.0 - alpha - beta
            let point = alpha * a + beta * b + y * c
            let color = material.diffuseColor
            { point = point; color = color; normal=alpha * v1 + beta * v2 + y * v3 }


let intersection shape ray = 
    match shape with
        | Sphere (center, radius, material) ->
            let s = ray.origin - center
            let rayDir = norm ray.direction
            let sv = Vector3D.DotProduct(s, rayDir)
            let ss = Vector3D.DotProduct(s,s)
            let discr = sv*sv - ss + radius * radius
            if discr < 0.0 then []
            else
                let normalAtTime t = norm ((pointAtTime ray t) - center)
                let (t1,t2) = (-sv + sqrt(discr), -sv - sqrt(discr))
                [ { normal = normalAtTime t1; point = pointAtTime ray t1; ray = ray; shape=shape; t = t1; material=material };
                  { normal = normalAtTime t2; point = pointAtTime ray t2; ray = ray; shape=shape; t = t2; material=material } ]
        | Triangle(a,b,c,v1,v2,v3,material) ->
            let (alpha,beta,y,t) = cramers a b c ray
            let n = alpha * v1 + beta * v2 + y * v3
            let p = pointAtTime ray t
            [ { normal = n; point = p; ray = ray; shape = shape; t = t; material=material } ]

let raytrace ray (scene : Scene) min max =
    List.filter (fun x -> x.t > min && x.t < max) (List.collect (fun x -> x) (List.map (fun x -> intersection x ray) scene.shapes))

let intersect2 ray (list:list<Shape>) min max = 
    List.filter (fun x -> x.t > min && x.t < max) (List.collect (fun x -> x) (List.map (fun x -> intersection x ray) list))

let getArea shape =
    match shape with
        | Sphere(center, radius, diffuseColor) ->
            4.0 * Math.PI * radius * radius
        | Triangle(a,b,c,v1,v2,v3,color) ->
            Vector3D.CrossProduct(v1, v2).Length / 2.0

let totalLightArea (lights:list<Light>) = 
    let areas = seq {
        for light in lights do
            yield getArea light.shape
    }
    Seq.sum areas

let computeLightIntervals (lights : list<Light>) =
    let areaTot = totalLightArea lights
    let rec foo (lights : list<Light>) lastEnding listofintervals =
        match lights with
            | [] -> listofintervals
            | car::cdr ->
                let area = getArea car.shape
                let percent = (area / areaTot) * 100.0
                let first = lastEnding
                let last = lastEnding + percent
                let intervals = {light = car; first = first; last = last}::listofintervals
                foo cdr last intervals
    foo lights 0.0 []


let getLight lightintervals (rand : Random) =
    let random = rand.NextDouble() * 100.0
    (Seq.find (fun x -> x.first <= random && x.last >= random) lightintervals).light

let directLighting (intersection : Intersection) (scene : Scene) rand (lightintervals : seq<LightInterval>) = 
    let light = getLight lightintervals rand
    let sample = getSampleFromShape light.shape rand
    let toLight = sample.point - intersection.point
    let L = norm toLight
    let tmin = 0.01
    let tmax = toLight.Length
    let shadowray = { origin = intersection.point; direction = L }
    if (raytrace shadowray scene tmin tmax).Length > 0 then Color.Zero
    else
        let areatot = totalLightArea scene.lights
        let kd = intersection.material.diffuseColor / Math.PI
        let g1 = Vector3D.DotProduct(sample.normal,-L)
        let g2 = Vector3D.DotProduct(intersection.normal, L)
        let G = Math.Abs(g1 * g2 / toLight.LengthSquared)
        kd * G * light.color * areatot

let getReflectedRay ray (inter:Intersection) = 
    let cl = - Vector3D.DotProduct(inter.normal, ray.direction)
    let Rl = ray.direction + (2.0 * inter.normal * cl)
    { origin=inter.point; direction=Rl }

let diffuse inter scene rand lightintervals ray shades =
    let direct = directLighting inter scene rand lightintervals
    let directionwd = generateWeightedDirection rand inter.normal
    let r = { origin = inter.point; direction = directionwd }
    direct + shades * (inter.material.diffuseColor / Math.PI)

let rec shade (ray : Ray) (scene:Scene) (rand:Random) (counter:int) (lightintervals : seq<LightInterval>) : Color=
    let lightInter = intersect2 ray (List.map ( fun (x:Light) -> x.shape) scene.lights) 0.1 100.0
    let shapeInter = intersect2 ray scene.shapes 0.1 100.0
    let lightshade = (fun (inter:Intersection) -> if counter = 0 then inter.material.diffuseColor else Color.Zero)
    let shapeshade = (fun (inter:Intersection) -> 
                        if counter >= 4 then Color.Zero 
                        else
                           let direct = directLighting inter scene rand lightintervals
                           let directionwd = generateWeightedDirection rand inter.normal
                           let randomray = { origin = inter.point; direction = directionwd }
                           let diffuseShades = (shade randomray scene rand (counter+1) lightintervals)
                           let diffuse = (direct + diffuseShades * (inter.material.diffuseColor / Math.PI)) * inter.material.diffusefactor

                           let reflectedray = getReflectedRay ray inter
                           let specular = (shade reflectedray scene rand (counter+1) lightintervals) * inter.material.reflectivity * inter.material.specularColor
                           diffuse + specular)
    match (lightInter.Length, shapeInter.Length) with
    | (x,0) when x > 0 -> lightshade (List.minBy (fun x -> x.t) lightInter)
    | (0,y) when y > 0 -> shapeshade (List.minBy (fun x -> x.t) shapeInter)
    | (x,y) when x > 0 && y > 0 ->  let lightmin = List.minBy (fun x -> x.t) lightInter
                                    let shapemin = List.minBy (fun x -> x.t) shapeInter
                                    if lightmin.t < shapemin.t then lightshade lightmin else shapeshade shapemin
    | (_,_) -> Color(0.0,0.0,0.0)

[<STAThread>]
do
    let width = 512
    let height = 512
    let samples = 150.0
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

    let color1 = Color(0.31,0.43,1.0)
    let color2 = Color(1.0,0.416,0.0)
    let material = { reflectivity=1.0; diffusefactor=0.0; diffuseColor=color1; specularColor=Color(1.0, 1.0, 1.0) }
    let material1 = { reflectivity=0.0; diffusefactor=1.0; diffuseColor=color2; specularColor=Color(1.0, 1.0, 1.0) }
    let material2 = { reflectivity=0.0; diffusefactor=1.0; diffuseColor=Color(0.5, 1.0, 0.1); specularColor=Color(1.0, 1.0, 1.0) }
    let material3 = { reflectivity=0.0; diffusefactor=1.0; diffuseColor=Color(1.0, 1.0, 1.0); specularColor=Color(1.0, 1.0, 1.0) }
    let lightmaterial = { reflectivity=0.0; diffusefactor=1.0; diffuseColor=Color(1.0, 1.0, 1.0); specularColor=Color(1.0, 1.0, 1.0) }

    // Sphere
    let sphere = Sphere(Vector3D(1.0, 1.0, 0.5), 1.0, material)
    let sphere2 = Sphere(Vector3D(-1.0, 1.0, -0.2), 0.4, material1)
    let sphere3 = Sphere(Vector3D(1.0, 1.0, 100.0), 98.0, material2)
    let sphere4 = Sphere(Vector3D(100.0, 1.0, 1.0), 97.0, material3)

    // Camera
    let camera = { position = Vector3D(-4.0, 5.0, -2.0); lookAt = Vector3D(1.0, 1.0, 1.0); lookUp = Vector3D(0.0, 1.0, 0.0) }

    // Scene
    let sun = Sphere(Vector3D(0.0,0.0,-5.0), 1.0, lightmaterial)
    let sun1 = Sphere(Vector3D(-3.5,1.0,-1.8), 1.0, lightmaterial)
    let light : Light = { shape=sun; color=Color(1.0,1.0,1.0) }
    let light1 : Light = { shape=sun1; color=Color(1.0,1.0,1.0) }
    let scene = { camera = camera; shapes = [sphere; sphere2; sphere3; sphere4]; ambientLight = Color(0.2, 0.2, 0.2); lights = [light; light1] }

    // Set up the coordinate system
    let n = norm (camera.position - camera.lookAt)
    let u = norm (Vector3D.CrossProduct(n, camera.lookUp))
    let v = norm (Vector3D.CrossProduct(n, u))
    let vpc = camera.position - n

    let rand = Random()
    
    let intervals = computeLightIntervals scene.lights
    let stopwatch = new Diagnostics.Stopwatch()

    stopwatch.Start()
    for x in 0..(width - 1) do
        for y in 0..(height - 1) do
            let colors = seq { 
                            for i in 1..int(samples) do
                                let dx = rand.NextDouble() - 0.5
                                let dy = rand.NextDouble() - 0.5
                                let rayPoint = vpc + (dx + float(x-width/2))*pw*u + (dy + float(y-height/2))*ph*v
                                let rayDir = norm (rayPoint - scene.camera.position)
                                let ray = { origin = scene.camera.position; direction = rayDir }
                                yield shade ray scene rand 0 intervals                      
                              }
            let color = ((Seq.fold (fun a b -> a + b) Color.Zero colors) * (1.0 / samples) * Math.PI).clip 
            bmp.SetPixel(x,y, Color.FromArgb(255, (int)(color.r*255.0), (int)(color.g*255.0), (int)(color.b*255.0)))
        Console.WriteLine(string(x))
    stopwatch.Stop()

    Console.WriteLine("Time to build bitmap: {0:####}.{1:##} sec", stopwatch.Elapsed.Seconds, stopwatch.Elapsed.Milliseconds)
           
    box.Image <- (bmp :> Image)

    bmp.Save(@"c:\users\mfh\desktop\output1.jpg")
    Application.Run(mainForm)