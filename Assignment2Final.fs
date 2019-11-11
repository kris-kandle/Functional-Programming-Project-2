// Learn more about F# at http://fsharp.org

type Part = string
type Task = string
type Cost = int
type Duration = int
type PartReg = Map<Part, Cost>
type TaskReg = Map<Task, Duration*Cost>

let preg1 = Map.ofList [("wheel", 50); ("saddle", 10); ("handlebars", 75); ("frame", 100); ("screw bolt", 5); ("nut", 3)];;
let treg1 = Map.ofList [("addWheels",(10,2)); ("addSaddle",(5,2)); ("addHandlebars",(6,1))];;


type WorkStation = Task * (Part*int) list
type AssemblyLine = WorkStation list
type Stock = Map<Part, int>

let ws1 = ("addWheels",[("wheel",2);("frame",1);("screw bolt",2);("nut",2)])
let ws2 = ("addSaddle",[("saddle",1);("screw bolt",1);("nut",1)])
let ws3 = ("addHandlebars",[("handlebars",1);("screw bolt",1);("nut",1)])
let al1 = [ws1; ws2; ws3];;


//Question 1
//Because we are looking at a list of parts, we need a recursive function to run through the pr with plist
let rec checkParts (pr:PartReg) plist =
    match plist with
    |(part,_)::xs when pr.ContainsKey(part) -> checkParts pr xs
    | []-> true 
    | _->false
    
//Checks first to see if the task of the workstation is in the tr, then calls the checkParts function for the parts
let wellDefWS (pr:PartReg) (tr:TaskReg) (ws:WorkStation) = 
    match ws with
    |(task,plist) when tr.ContainsKey(task) -> checkParts pr plist
    |_->false

//Question 2
// Uses wellDefWS on all ws in the assembly line from x::xs. Declares true when true for all ws
let rec wellDefAL (pr:PartReg) (tr:TaskReg) (al:AssemblyLine) = 
    match al with
    | ws::xs when (wellDefWS pr tr ws) -> wellDefAL pr tr xs
    |[] -> true
    |_->false

//Helper Functions to access task list and part list of workstation through assembly line
let alTaskList al = List.fold (fun acc elem -> acc @ [fst elem]) [] al
let alPartList al = List.fold (fun acc elem -> acc @ [snd elem]) [] al

//Q3 Type - val longestDuration : al:('a * 'b) list -> treg:Map<'a,('c * 'd)> -> 'c when 'a : comparison and 'c : comparison and 'd : comparison
//uses Map.filter to remove the strings in treg, then re inserts treg into a list, and then uses List.max to determine the longest duration of time in a workstation
let longestDuration al treg =
    let alTsklist = alTaskList al
    let treg1 = Map.filter (fun key _ -> List.contains(key) alTsklist) treg
    let treg1 = Map.toList treg1
    let result = List.max(treg1)
    fst (snd result)

let test3 = longestDuration al1 treg1 = 10

//Q4 Type - preg:Map<'a,int> -> al:('b * ('a * int) list) list -> int when 'a : comparison
//Uses List.fold to run through the assembly line list, uses Map.find to find a part in the preg and 
//multiplies the cost by its number of parts, then uses List.concat to combine all the costs together
let partCostAL preg al = 
    let parts = alPartList al
    List.fold (fun acc (x,y) -> acc + (Map.find x preg*y)) 0 (List.concat parts)


let test4 = partCostAL preg1 al1 = 317

//Q5 Type - treg:Map<'a,(int * int)> -> al:('a * 'b) list -> int * intwhen 'a : comparison
let sumTouple(x1,y1)(x2,y2) = (x1+x2,y1+y2) //Helper function that sums two touples
let prodDurCost treg al = List.fold(fun acc (Task,_) -> sumTouple(Map.find Task treg)acc)(0,0) al //Takes treg and al and folds a function of acc and (Task,_) 
                                                                                                  //and puts it in sumTouple(Map.find Task treg)acc) which sums the durations and the costs 
                                                                                                  //and pusts them in a touple, sets initial to (0,0) 
                                                                                                    

let test5 = prodDurCost treg1 al1 = (21,5)

//Q6 
type Stock = Map<Part, int>

//let toStock al = 