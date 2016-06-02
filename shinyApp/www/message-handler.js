// This recieves messages of type "savemessage" from the server.
Shiny.addCustomMessageHandler("savemessage",
  function(message) {
    alert(JSON.stringify(message));
  }
);
