function newNote()
{
    console.log("newNote");
    
    var newNoteID = "note" + $$('.note').length;
    
    var t = new Template(
        '<div id="#{note_id}" class="note" onclick="$(this).toggleClassName(\'noteselected\')">' +
        '<div class="noteheader">' + 
        '</div>' +
        '<textarea class="notebody" onchange="noteChanged(this.parentNode)"></textarea>' +
        '</div>'
    );
    
    console.log("Evaluating: %s", t.evaluate({ note_id: newNoteID}));
    $('note_board').insert(t.evaluate({ note_id: newNoteID}));
    
    new Draggable(newNoteID, {
		onEnd: function() {
			var e = $(newNoteID);
			console.log('New Position: (%d, %d), (%d, %d)', e.offsetLeft, e.offsetTop, e.offsetWidth, e.offsetHeight);
		}
	});
}

function saveNotes()
{
    console.log("saveNotes");
    
    var notes = [];
    
    $$('.note').each(function(n) {        
        var note = {
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
    return notes;
}

function noteChanged()
{
    console.log("noteChanged");
}