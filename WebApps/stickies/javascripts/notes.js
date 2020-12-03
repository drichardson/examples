console.log("notes.js loading");

function loadNotes()
{
    console.log("loadNotes called");
    new Ajax.Request("/notes_rpc", {
       method: 'get',
       onSuccess: function(transport) {
           console.log("Response text is '%s'", transport.responseText);
           
           notes = eval( '(' + transport.responseText + ')');           
           loadNotesWithData(notes);
       },
       onFailure: function() {
           console.log("loadNotes failed");
       }
    });
}

function makeNoteNode(container, id, text, top, left, width, height)
{
    var note_div = document.createElement("div");
    var note_header = document.createElement("div");
    var note_textarea = document.createElement("textarea");
    
    note_div.insert(note_header);
    note_div.insert(note_textarea);
    
    note_div.addClassName("note");
    note_header.addClassName("noteheader");
    note_textarea.addClassName("notebody");
    
    note_div.setAttribute("id", id);
    note_textarea.value = text;
    
    note_div.setStyle({
        left: left + 'px',
        top: top + 'px',
        width: width + 'px',
        height: height + 'px'
    });
    
    container.insert(note_div); // Need to insert into document before using Draggable.
    
    new Draggable(id, {
   		onEnd: function() {
   			console.log('New Position: (%d, %d), (%d, %d)', note_div.offsetLeft, note_div.offsetTop, note_div.offsetWidth, note_div.offsetHeight);
   		}
   	});
    
    return note_div;
}

function loadNotesWithData(notes)
{
    var b = $('note_board');
    notes.each(function(n) {
        console.log("Loading note: %s", n.pk);
        var nn = makeNoteNode(b, "note" + n.pk, n.text, n.top, n.left, n.width, n.height);
        nn.setAttribute("primaryKey", n.pk);
    });
}

function newNote()
{
    console.log("newNote");
    
    var newNoteID = "tmp_note" + $$('.note').length;
    
    makeNoteNode($('note_board'), newNoteID, "", 10, 10, 200, 200);
}

function saveNotes()
{
    console.log("saveNotes");
    
    var notes = [];
    
    $$('.note').each(function(n) {        
        var note = {
            pk: n.getAttribute("primaryKey"),
            top: n.offsetTop,
            left: n.offsetLeft,
            width: n.offsetWidth,
            height: n.offsetHeight,
            text: n.select('.notebody')[0].value
        };
        
        notes.push(note);
    });
    
    // TODO: Send JSON request to web server.
    console.log("  saving: " + notes.toJSON());
    
    
    new Ajax.Request("/notes_rpc", {
       method: 'post',
       parameters: {
           notes: notes.toJSON()
       },
       onSuccess: function(transport) {
           console.log("Response text is '%s'", transport.responseText);
           
           notes = eval( '(' + transport.responseText + ')');
       },
       onFailure: function() {
           console.log("saveNotes failed");
       }
    });
    return notes;
}

function noteChanged()
{
    console.log("noteChanged");
}