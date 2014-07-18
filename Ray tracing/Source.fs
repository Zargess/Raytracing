module Source

open System
open System.Windows.Media.Media3D
open System.Drawing
open System.Windows.Forms

(*****************************************)
(* Some type definitions for convenience *)
type Color(r: float, g: float, b:float) =
    member this.r = r
    member this.g = g
    member this.b = b
    static member ( * ) (c1:Color, c2:Color) =
        Color (c1.r*c2.r, c1.g*c2.g, c1.b*c2.b)
    static member ( * ) (c:Color, s:float) =
        Color (c.r*s, c.g*s, c.b*s)
    static member ( + ) (c1:Color, c2:Color) = // there must be a less ugly way to do this
        let (r,g,b) = (c1.r + c2.r, c1.g + c2.g, c1.b + c2.b)
        let r = match c1.r+c2.r with
                |_ when c1.r+c2.r > 1.0 -> 1.0
                |_ -> c1.r+c2.r
        let g = match c1.g+c2.g with
                |_ when c1.g+c2.g > 1.0 -> 1.0
                |_ -> c1.g+c2.g
        let b = match c1.b+c2.b with
                |_ when c1.b+c2.b > 1.0 -> 1.0
                |_ -> c1.b+c2.b
        Color (r,g,b)
    static member Zero = Color(0.0,0.0,0.0)

// Geometry stuff
type Time     = float
type Ray      = { origin:Point3D; direction:Vector3D }
type Material = { reflectivity:float; diffuseColor:Color; specularColor:Color; shininess:float}
type Center   = Point3D
type Radius   = float
type Normal   = Vector3D


(*****************************************)
(*                Shapes!                *)
// Define a sphere as a center, a radius and a material.
// Plane is a normal, a point on the plane and a material.
// Bound plane is defined as two points representing two opposite corners, a normal and a material
type Shape =
    | Sphere     of Center * Radius  * (Point3D -> Material)
    | Plane      of Normal * Point3D * (Point3D -> Material)
    | BoundPlane of Normal * Point3D * Point3D * (Point3D -> Material)

// Scene stuff
type Camera   = { position:Point3D; lookAt:Point3D; lookUp:Vector3D }
type Light    = { position:Point3D; color:Color }
type Lighting = { ambientLight:Color; lights:list<Light> }
type Scene    = { camera:Camera; lighting:Lighting; shapes:list<Shape> }


(*****************************************)
(*     Some simple utility functions     *)
/// Get the unit vector of a given vector
let norm (v:Vector3D) = 
    let abs = sqrt (v.X * v.X + v.Y * v.Y + v.Z * v.Z)
    v / abs

/// Get the position of a ray at a given time
let pointAtTime ray time =
    ray.origin + time * ray.direction

// Turn a three-float RGB color into a 4-tuple ARGB value
let argbFromColor (color:Color) =
    (255,(int(color.r*255.0)), (int(color.g*255.0)), (int(color.b*255.0)))


(*****************************************)
(*            Some constants             *)
// Epsilon is used for comparing equality with floating point numbers to avoid rounding errors
let Epsilon = 0.0001
// Background color
let Background = Color(0.0,0.0,0.0)
// FadeDistance determines the distance at which rays fade to background color
let FadeDistance = 30.0
// MaxReflections determines the number of times a light ray can bounce before we kill it. This
// prevents infinite relfections
let MaxReflections = 10


(*****************************************)
(*     The intersecetion algorithms      *)
// To make things a little simpler, we define an Intersection type which holds all of the
// information we need about a given intersection in the case we need to use this particular
// intersection for a colour calculation
type Intersection = { normal:Normal; point:Point3D; ray:Ray; material:Material }

// Returns the list of tuples of intersection points of the given ray with the given shape
// with the time value at which the intersection occurred.
let rec intersectShape shape ray =
    match shape with
    | Sphere (center, radius, material ) -> // intersect a sphere
        let s = ray.origin - center
        let rayDir = norm ray.direction
        let sv = Vector3D.DotProduct(s,rayDir)
        let ss = Vector3D.DotProduct(s,s)
        let discr = sv*sv - ss + radius*radius
        if discr < 0.0 then []
        else
            let normalAtTime t = norm (pointAtTime ray t - center)
            let (t1,t2) = (-sv + sqrt(discr), -sv - sqrt(discr))
            [ (t1, { normal = normalAtTime t1; point = pointAtTime ray t1; ray = ray; material = material (pointAtTime ray t1) });
              (t2, { normal = normalAtTime t2; point = pointAtTime ray t2; ray = ray; material = material (pointAtTime ray t2) }) ]
    
    | Plane  (normal, point,  material ) -> // intersect a plane
        let v = ray.direction
        let N = norm normal
        let vN = Vector3D.DotProduct(v,N)
        if vN = 0.0 then []
        else
            let t = Vector3D.DotProduct((point - ray.origin), N) / vN
            let intPoint = pointAtTime ray t
            [ (t , { normal = normal; point = pointAtTime ray t; ray = ray; material = material intPoint } ) ]

    | BoundPlane (normal, point0, point1, material) -> // intersect a bound plane
        let plane = Plane (normal, point0, material)
        let planeIntersects = intersectShape plane ray
                              |> List.filter (fun x -> fst x > Epsilon)
        let onBoundPlane intersection = // there's probably a less ugly way to do this
            intersection.point.X >= min (point0.X-Epsilon) (point1.X-Epsilon) &&
            intersection.point.Y >= min (point0.Y-Epsilon) (point1.Y-Epsilon) &&
            intersection.point.Z >= min (point0.Z-Epsilon) (point1.Z-Epsilon) &&
            intersection.point.X <= max (point0.X+Epsilon) (point1.X+Epsilon) &&
            intersection.point.Y <= max (point0.Y+Epsilon) (point1.Y+Epsilon) &&
            intersection.point.Z <= max (point0.Z+Epsilon) (point1.Z+Epsilon) 
        let result = List.filter (fun x -> onBoundPlane (snd x)) planeIntersects
        result


// Figures out if a particular point is in shadow from a particular light or not
let shadowAt intersection light scene =
    let lightDir = norm (light.position - intersection.point)
    let ray = { origin = intersection.point; direction = lightDir }
    let lightDirs = List.map (fun x -> (norm (x.position - intersection.point))) scene.lighting.lights
    let list = scene.shapes
               |> List.collect (fun x -> intersectShape x ray)
               |> List.filter (fun x -> (fst x) > Epsilon)
    not list.IsEmpty

// Gets the color from a particular light hitting a particular intersection point
let colorFromLight light intersection scene =
    let kd = intersection.material.diffuseColor
    let ks = intersection.material.specularColor
    let Is = light
    let L = norm (light.position - intersection.point)
    let V = scene.camera.position - intersection.point
    let H = norm (L + V)
    let normal = norm intersection.normal
    let HN = Vector3D.DotProduct(H,normal)
    let HN = match HN with
             |_ when HN < 0.0 -> 0.0
             |_ -> Math.Pow(HN, intersection.material.shininess)
    let LN = Vector3D.DotProduct(L,normal)
    let LN = match LN with
             |_ when LN < 0.0 -> 0.0
             |_ -> LN
    let phongFactor = ks * HN
    let diffuseFactor = kd * LN
    match shadowAt intersection light scene with
    | true -> Is.color * phongFactor
    | false -> Is.color * (diffuseFactor + phongFactor)

// calculates the color to render at a particular intersection point.
let colorAt intersection scene =
    let kd = intersection.material.diffuseColor
    let Ia = scene.lighting.ambientLight
    let ambient = kd * Ia
    let reflectedCols = List.map (fun x -> colorFromLight x intersection scene) scene.lighting.lights
    ambient + List.sum reflectedCols

// recursively casts rays to determine the color a given ray should register
let rec castRay ray scene numReflections = 
    let intersects = scene.shapes 
                     |> List.collect (fun x -> intersectShape x ray)
                     |> List.filter  (fun x -> fst x > Epsilon)
    match intersects with
    | [] -> Background
    | _  -> let (time, intersection) = List.minBy (fun x -> (fst x))  intersects
            let colorAtIntersection = colorAt intersection scene
            let reflectDir = (ray.direction - 2.0 * norm ((Vector3D.DotProduct(ray.direction, intersection.normal)) * intersection.normal))
            let newRay = { origin = intersection.point; direction = reflectDir }
            match time with
            | _ when time > FadeDistance -> Background
            | _ -> match numReflections with
                   | _ when numReflections < MaxReflections -> 
                          ((colorAtIntersection * (1.0-intersection.material.reflectivity)) + 
                            ((castRay newRay scene (numReflections+1)) * intersection.material.reflectivity)) * 
                            ((FadeDistance-time)/FadeDistance)
                   | _ -> (colorAtIntersection * (1.0-intersection.material.reflectivity))
  
(* Main thread of execution  
   Most of the code in here is just ugly setup code for the scene and camera *)
[<STAThread>]
let getPic =  
    // Make ourselves a canvas
    let width = 640
    let height = 480

    // Vertical and horizontal field of view
    let hfov = System.Math.PI/3.2
    let vfov = hfov * float(height)/float(width)

    // Pixel width and height
    let pw = 2.0 * System.Math.Tan(float(hfov/2.0))/float(width)
    let ph = 2.0 * System.Math.Tan(float(vfov/2.0))/float(height)    

    let bmp = new Bitmap(width,height)

    // Some colors, including one given as a higher order function
    let color1 = Color(0.31,0.43,1.0)
    let color2 = Color(1.0,0.416,0.0)
    let lightColor = Color(0.5,0.5,0.5)
    let checkerFunc (p:Point3D) size =
        let (x,y,z) = (p.X, p.Y, p.Z)
        let jump x y z =
            let j = int((int(Math.Abs(float(x))/size) + int(Math.Abs(float(y))/size) + int(Math.Abs(float(z))/size)) % 2)
            match Math.Abs(j) with
            |_ when j = 1 -> Color(0.0,0.0,0.0)
            |_ -> Color(1.0,1.0,1.0)
        jump x y z

    // Some materials
    let material1 = { reflectivity=0.2; diffuseColor=color1; specularColor = Color(1.0,1.0,1.0); shininess=500.0 }
    let material2 = { reflectivity=0.2; diffuseColor=color2; specularColor = Color(1.0,1.0,1.0); shininess=500.0 }

    // Spheres
    let sphere0 = Sphere(Point3D(0.0,0.0,0.0), 0.7, (fun x -> material1))
    //let sphere1 = Sphere(Point3D(0.0,-0.7,0.0), 0.5, (fun x -> material1))
    let sphere1 = Sphere(Point3D(1.5,0.2,1.5), 0.5, (fun x -> material2))
    let sphere2 = Sphere(Point3D(-1.5,0.2,1.5), 0.5, (fun x -> material2))
    let sphere3 = Sphere(Point3D(1.5,0.2,-1.5), 0.5, (fun x -> material2))
    let sphere4 = Sphere(Point3D(-1.5,0.2,-1.5), 0.5, (fun x -> material2))
    
    // Floor plane
    let floor   = Plane(Vector3D(0.0,-1.0,0.0), Point3D(0.0,1.0,0.0), (fun x -> { reflectivity = 0.2; diffuseColor = checkerFunc x 1.5; specularColor = Color(1.0,1.0,1.0); shininess=500.0 }))

    // Janky way of constructing a box
    let top   = BoundPlane(Vector3D(0.0,-1.0,0.0), Point3D(-2.0,0.7,-2.0), Point3D(2.0,-0.7,2.0), (fun x -> material1))
    let side1 = BoundPlane(Vector3D(1.0,0.0,0.0), Point3D(2.0,1.0,-2.0), Point3D(2.0,0.7,2.0),    (fun x -> material1))
    let side2 = BoundPlane(Vector3D(-1.0,0.0,0.0), Point3D(-2.0,1.0,-2.0), Point3D(-2.0,0.7,2.0), (fun x -> material1))
    let side3 = BoundPlane(Vector3D(0.0,0.0,1.0), Point3D(-2.0,1.0,2.0), Point3D(2.0,0.7,2.0),    (fun x -> material1))
    let side4 = BoundPlane(Vector3D(0.0,0.0,-1.0), Point3D(-2.0,1.0,-2.0), Point3D(2.0,0.7,-2.0), (fun x -> material1))
    
    // lighting
    let light1 = { position = Point3D(5.0,-2.0,0.0); color = Color(0.9,0.9,0.9) }
    let light2 = { position = Point3D(-5.0,-2.0,0.0); color = Color(0.7,0.7,0.7) }
    let light3 = { position = Point3D(0.0,-2.0,5.0); color = Color(0.3,0.3,0.3) }
    let light4 = { position = Point3D(0.0,-2.0,-5.0); color = Color(0.2,0.2,0.2) }
    let lighting = { ambientLight=lightColor; lights=[light1;light2;light3;light4] }
    
    // camera
    let camera = { position=Point3D(1.0,-1.7,5.0); lookAt=Point3D(0.0,0.8,0.0); lookUp=Vector3D(0.0,1.0,0.0) }
    
    // scene
    let scene = { camera=camera; lighting=lighting; shapes = [sphere0;floor;top;side1;side2;side3;side4;sphere1;sphere2;sphere3;sphere4;] }

    // set up the coordinate system
    let n = norm (scene.camera.position - scene.camera.lookAt)
    let u = norm (Vector3D.CrossProduct(scene.camera.lookUp, n))
    let v = norm (Vector3D.CrossProduct(n, u))
    let vpc = scene.camera.position - n

    // render the scene
    let stopwatch = new System.Diagnostics.Stopwatch()
    Console.WriteLine("Building scene at resolution {0}*{1}...", width, height)
    stopwatch.Start()

    let image = Array.Parallel.init width  (fun x ->
                Array.init height (fun y -> 
                    let rayPoint = vpc + float(x-width/2)*pw*u + float(y-height/2)*ph*v
                    let rayDir = norm (rayPoint - scene.camera.position)
                    let ray = { origin = scene.camera.position; direction = rayDir }
                    let color = castRay ray scene 0
                    let (a,r,g,b) = argbFromColor(color)
                    Color.FromArgb(a,r,g,b)))

    stopwatch.Stop()
    Console.WriteLine("Time to render scene: {0:####}.{1:##} sec", stopwatch.Elapsed.Seconds, stopwatch.Elapsed.Milliseconds)
    stopwatch.Reset()

    // build the bitmap from the rendered scene
    stopwatch.Start()
    for x in 0..(image.Length-1) do
        for y in 0..(image.[x].Length-1) do 
            bmp.SetPixel(x,y,image.[x].[y])
    stopwatch.Stop();
    Console.WriteLine("Time to build bitmap: {0:####}.{1:##} sec", stopwatch.Elapsed.Seconds, stopwatch.Elapsed.Milliseconds)
    
    // save the bitmap to disk.
    bmp.Save("FRayOutput.jpg");
    bmp