
 
function phaseOne(x) {
    const stimuli = document.getElementsByClassName("stimulus");
    for (let i = 0; i < stimuli.length; i++) {
        stimuli[i].style.visibility = "visible";
        stimuli[i].style.pointerEvents = "all";
    }
    const match = document.getElementById("match.1");
    const matches = match.children;
    for (let i = 0; i < matches.length; i++) {
        matches[i].style.visibility = "hidden";
        matches[i].setAttribute("pointer-events", "none");
    }
}

function phaseTwo(x) {
    const stimuli = document.getElementsByClassName("stimulus");
    for (let i = 0; i < stimuli.length; i++) {
        stimuli[i].style.visibility = "hidden";
        stimuli[i].style.pointerEvents = "none";
    }
    const match = document.getElementById("match.1");
    const matches = match.children;
    for (let i = 0; i < matches.length; i++) {
        matches[i].style.visibility = "visible";
        matches[i].setAttribute("pointer-events", "stroke");
    }
}
