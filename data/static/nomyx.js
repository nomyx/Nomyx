function toggle_visibility(id_events, id_show) 
{ 
   var events = document.getElementById(id_events);
   var show = document.getElementById(id_show);
   if (show.innerHTML == '[Show]') 
   {
      events.style.display = 'block'; 
      show.innerHTML = '[Hide]';
   }
   else   
   {
      events.style.display = 'none'; 
      show.innerHTML ='[Show]';
   }
}
