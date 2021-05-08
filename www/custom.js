
// define debounce function
const debounce = function(func, delay) {
  let timeout;

  return function executed(...args) {
    const later = function() {
      clearTimeout(timeout);
      func(...args);
    };

    clearTimeout(timeout);
    timeout = setTimeout(later, delay);

  };

};

// Create array of letters
const alph = [..."abcdefghijklmnopqrstuvwxyz"];
// Create array of spelled out numbers
const nums = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"];

function updateColorIds() {
// Create array of all relevant ids and then 
// convert into an array with elements "name" and "value"
colorIds = [...alph, ...nums].map(cId => {

let thing = {}

thing["name"] = cId
thing["color"] = $('#' + cId).val()

return thing
})
console.log(colorIds[0])
}


function updateDisplayedText() {
  
  // create an array where every character in the coloredText div is its own element
  displayed = $('#coloredText').html().split('');
  // make that array have a span with its own class
  newDisplay = displayed.map(el => '<span class = "displayed-' + el + '">' + el + '</span>');
  // update DOM with newDisplay
  $('#coloredText').html(newDisplay);
  
  // For each element with class "displayed-" within the coloredText div, change the 
  // color for the class
  colorIds.map(cId => {$('#coloredText .displayed-' + cId.name).css("color", cId.color)})
}

$(document).ready(function() {
  
  // Initialize color ids
  updateColorIds();
  
  // Initialize text with default colors
  updateDisplayedText();
    
  // Whenever one of the inputs changes -- and none have been changed for
  // one thousand milliseconds, update the colorIds
  $('#colorSelectors input').on("change", debounce(updateColorIds, 1000))
  
  // Whenever someone adds new text to display and they haven't typed
  // anything for 1000 milliseconds, update the colors of the displayed text
  $('#displayText').on("keyup", debounce(updateDisplayedText, 300))
  
});

