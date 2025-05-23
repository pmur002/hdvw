
function(nstim) {

    let numStimClicked = 0;
    let stimCols = [];
    let numMatchClicked = 0;
    let matchCols = [];
    let error = 0;

    const svg = document.getElementById("memory" + nstim);

    function angleDiff(x, y) {
        var diff = Math.abs(x - y);
        if (diff > 180)
            diff = 360 - diff;
        return diff;
    }

    function visID(id, visibility, fix=true) {
        if (fix) {
            id = "memory" + nstim + id;
        }
        const elt = svg.getElementById(id);
        elt.style.visibility = visibility;
    }
    
    function showID(id, fix=true) { visID(id, "visible", fix); }
    function hideID(id) { visID(id, "hidden"); }

    function visClass(cl, visibility) {
        const elts = svg.getElementsByClassName(cl);
        for (let i = 0; i < elts.length; i++) {
            elts[i].style.visibility = visibility;
        }
    }

    function showClass(id) { visClass(id, "visible"); }
    function hideClass(id) { visClass(id, "hidden"); }

    return {

        phaseOne(x) {
            /* Initialisation */
            numStimClicked = 0;
            stimCols.length = 0;
            numMatchClicked = 0;
            matchCols.length = 0;
            error = 0;
            /* Show random stimuli */
            for (let i = 0; i < nstim; i++) {
                const stimuli = svg.getElementsByClassName("stimulus");
                const n = stimuli.length;
                const min = i*(n/nstim);
                const max = min + n/nstim - 1;
                const which = Math.floor(Math.random()*(max - min + 1) + min);
                showID(stimuli[which].id, fix=false);
            }
            /* Hide match */
            hideID("mask.1");
            hideID("match.1");
            hideClass("matchMark");
            hideClass("stimMark");
            hideID("error.1");
            hideID("play-again.1");
        },
        
        phaseTwo(x) {
            const clickedCol = parseInt(x.id.replaceAll(/.+-|[.].+/g, ""));
            var oldStim = false;
            if (numStimClicked > 0) {
                for (let i = 0; i < numStimClicked; i++) {
                    if (clickedCol == stimCols[i]) {
                        oldStim = true;
                    }
                }
            }
            if (!oldStim) {
                stimCols[numStimClicked] = clickedCol;
                numStimClicked = numStimClicked + 1;
            }
            if (numStimClicked == nstim) {
                /* Hide stimuli */
                hideClass("stimulus");
                /* Show match */
                showID("mask.1");
                showID("match.1");
            }
        },
        
        phaseThree(x) {
            matchCols[numMatchClicked] = parseInt(x.id.replace(/.+[.]/, ""));
            /* Show match mark */
            showID("matchMark-" + matchCols[numMatchClicked] + ".1");
            const thisError = angleDiff(matchCols[numMatchClicked], 
                                        stimCols[numMatchClicked]);
            if (thisError > error) {
                error = thisError;
            }
            numMatchClicked = numMatchClicked + 1;
            if (numMatchClicked == nstim) {
                /* Show stimulus marks */
                for (let i = 0; i < nstim; i++) {
                    showID("stimMark-" + stimCols[i] + ".1");
                }
                /* Show error */
                errorMsg = 
                    svg.getElementById("memory" + nstim + 
                                       "error.1.1.tspan.1");
                errorMsg.textContent = "Error = " + error.toFixed(1);
                showID("error.1");
                /* Show play again */
                showID("play-again.1");
            }
        }
    };
}

