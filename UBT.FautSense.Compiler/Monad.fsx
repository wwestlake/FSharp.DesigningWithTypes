

type Label = string
type VitalForce = {units: int}

let getVitalForce vitalForce =
    let oneUnit = {units = 1}
    let remaining = {units = vitalForce.units - 1}
    oneUnit, remaining

type DeadLeftLeg = DeadLeftLeg of Label
type LiveLeftLeg = LiveLeftLeg of Label * VitalForce
type MakeLiveLeftLeg = DeadLeftLeg -> (VitalForce -> LiveLeftLeg * VitalForce)
type DeadLeftBrokenArm = DeadLeftBrokenArm of Label 
type LiveLeftBrokenArm = LiveLeftBrokenArm of Label * VitalForce
type LiveLeftArm = LiveLeftArm of Label * VitalForce
type HealBrokenArm = LiveLeftBrokenArm -> LiveLeftArm 

type DeadRightLowerArm = DeadRightLowerArm of Label 
type DeadRightUpperArm = DeadRightUpperArm of Label 

type LiveRightLowerArm = LiveRightLowerArm of Label * VitalForce
type LiveRightUpperArm = LiveRightUpperArm of Label * VitalForce

type LiveRightArm = {
    lowerArm : LiveRightLowerArm
    upperArm : LiveRightUpperArm
    }

let armSurgery lowerArm upperArm =
    {lowerArm=lowerArm; upperArm=upperArm}


type M<'LiveBodyPart> =
    M of (VitalForce -> 'LiveBodyPart * VitalForce)

let makeLiveLeftLegM deadLeftLeg =
    let becomeAlive vitalForce =
        let (DeadLeftLeg label) = deadLeftLeg
        let oneUnit, remainingVitalForce = getVitalForce vitalForce
        let liveLeftLeg = LiveLeftLeg (label, oneUnit)
        liveLeftLeg, remainingVitalForce
    M becomeAlive

let runM (M f) vitalForce = f vitalForce

let healBrokenArm (LiveLeftBrokenArm (label,vf)) = LiveLeftArm (label,vf)




let makeHealedLeftArm brokenArmM = 

    // create a new inner function that takes a vitalForce parameter
    let healWhileAlive vitalForce = 
        // run the incoming brokenArmM with the vitalForce 
        // to get a broken arm
        let brokenArm,remainingVitalForce = runM brokenArmM vitalForce 
        
        // heal the broken arm
        let healedArm = healBrokenArm brokenArm

        // return the healed arm and the remaining VitalForce
        healedArm, remainingVitalForce

    // wrap the inner function and return it
    M healWhileAlive  


let mapM f bodyPartM = 

    // create a new inner function that takes a vitalForce parameter
    let transformWhileAlive  vitalForce = 
        let bodyPart,remainingVitalForce = runM bodyPartM  vitalForce 
    
        // heal the broken arm using passed in f
        let updatedBodyPart  = f bodyPart
        updatedBodyPart, remainingVitalForce

    M transformWhileAlive   

let healBrokenArmM = mapM healBrokenArm




let isEven x = (x%2 = 0)
let isEvenM = mapM isEven

let makeLiveLeftBrokenArm deadLeftBrokenArm = 
    let (DeadLeftBrokenArm label) = deadLeftBrokenArm
    let becomeAlive vitalForce = 
        let oneUnit, remainingVitalForce = getVitalForce vitalForce 
        let liveLeftBrokenArm = LiveLeftBrokenArm (label,oneUnit)
        liveLeftBrokenArm, remainingVitalForce    
    M becomeAlive

/// create a dead Left Broken Arm
let deadLeftBrokenArm = DeadLeftBrokenArm "Victor"

/// create a M<BrokenLeftArm> from the dead one
let leftBrokenArmM = makeLiveLeftBrokenArm deadLeftBrokenArm 

let leftArmM = leftBrokenArmM |> mapM healBrokenArm 

let deadLeftLeg = DeadLeftLeg "Boris"
let leftLegM = makeLiveLeftLegM deadLeftLeg 
let vf = {units = 10}

let liveLeftLeg, remainingAfterLeftLeg = runM leftLegM vf  

printfn "%A - %A" liveLeftLeg remainingAfterLeftLeg

let liveLeftArm, remainingAfterLeftArm = runM leftArmM vf

printfn "%A - %A" liveLeftArm remainingAfterLeftArm

/// convert a M<LiveRightLowerArm> and M<LiveRightUpperArm> into a M<LiveRightArm>
let makeArmSurgeryM_v1 lowerArmM upperArmM =

    // create a new inner function that takes a vitalForce parameter
    let becomeAlive vitalForce = 
        // run the incoming lowerArmM with the vitalForce 
        // to get the lower arm
        let liveLowerArm,remainingVitalForce = runM lowerArmM vitalForce 
        
        // run the incoming upperArmM with the remainingVitalForce 
        // to get the upper arm
        let liveUpperArm,remainingVitalForce2 = runM upperArmM remainingVitalForce 

        // do the surgery to create a liveRightArm
        let liveRightArm = armSurgery liveLowerArm liveUpperArm

        // return the whole arm and the SECOND remaining VitalForce
        liveRightArm, remainingVitalForce2  
          
    // wrap the inner function and return it
    M becomeAlive  


let map2M f m1 m2 =
    let becomeAlive vitalForce = 
        let v1,remainingVitalForce = runM m1 vitalForce 
        let v2,remainingVitalForce2 = runM m2 remainingVitalForce  
        let v3 = f v1 v2
        v3, remainingVitalForce2    
    M becomeAlive  













