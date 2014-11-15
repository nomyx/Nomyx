function toggle_visibility(id_events, id_show) 
{ 
   var events = document.getElementById(id_events);
   var show = document.getElementById(id_show);
   if (show.innerHTML == '[Click to show]') 
   {
      events.style.display = 'block'; 
      show.innerHTML = '[Click to hide]';
   }
   else   
   {
      events.style.display = 'none'; 
      show.innerHTML ='[Click to show]';
   }
}

//toggle visibility for overlapping divs
//all elements with the class name will be pushed back, 
//while only the element with the id name is put on front
function div_visibility(id_box, class_box, id_button, class_button)
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
