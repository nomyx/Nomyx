function toggle_visibility(id_events, id_show) 
{
   var events = document.getElementById(id_events);
   var show = document.getElementById(id_show);
   if (show.innerHTML == 'Click to show') 
   {
      events.style.display = 'block'; 
      show.innerHTML = 'Click to hide';
   }
   else   
   {
      events.style.display = 'none'; 
      show.innerHTML ='Click to show';
   }
}

//toggle visibility for overlapping divs
//all elements with the class name will be pushed back, 
//while only the element with the id name is put on front
function divBoxvisibility(id_box, class_box, id_button, class_button)
{
   //push back every divs
   var boxes = document.getElementsByClassName(class_box);
   for (i = 0; i < boxes.length; i++) {
      boxes[i].style.display = 'none';
   }

   //pull front our div
   var myBox = document.getElementById(id_box);
   myBox.style.display = 'inline';

   //unbold the text in the buttons
   var buttons = document.getElementsByClassName(class_button);
   for (i = 0; i < buttons.length; i++) {
      buttons[i].style.fontWeight = 'normal';
   }

   //pull front our div
   var myButton = document.getElementById(id_button);
   myButton.style.fontWeight = 'bold';

}

function setDivVisibility(gn, boxName, className) {

   var idBox       = gn + "IdBox" + boxName;
   var classBox    = gn + "ClassBox" + className;
   var idButton    = gn + "IdButton" + boxName;
   var classButton = gn + "ClassButton" + className;

   divBoxvisibility(idBox, classBox, idButton, classButton);
}

function setDivVisibilityAndSave(gn, boxName, className) {

   setDivVisibility(gn, boxName, className);
   setCookie("GameBox", gn + "," + boxName + "," + className);
}

function loadDivVisibility() {

   val = getCookie("GameBox");
   vals = val.split(',');
   setDivVisibility(vals[0], vals[1], vals[2]);

   console.log(vals[0] + " " + vals[1] + " " + vals[2]);
}

function setCookie(cname, cvalue) {
    var d = new Date();
    d.setTime(d.getTime() + (365*24*60*60*1000));
    var expires = "expires="+d.toUTCString();
    document.cookie = cname + "=" + encodeURIComponent(cvalue) + "; " + expires;
} 

function getCookie(cname) {
    var name = cname + "=";
    var ca = document.cookie.split(';');
    for(var i=0; i<ca.length; i++) {
        var c = ca[i];
        while (c.charAt(0)==' ') c = c.substring(1);
        if (c.indexOf(name) != -1) return decodeURIComponent(c.substring(name.length,c.length));
    }
    return "";
} 
