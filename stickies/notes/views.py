from google.appengine.api import users
from google.appengine.ext import webapp
from google.appengine.ext import db
from google.appengine.ext.webapp import template
from django.utils import simplejson

import os
import logging
from notes.models import Note

MaximimNotesPerPage=100

class NotesPage(webapp.RequestHandler):
    def get(self):
        user = users.get_current_user()
        if user:
            path = os.path.join(os.path.dirname(__file__), 'templates/notes.html')
            #logging.debug("notes template path is %s", path)
            f = open(path)
            self.response.out.write(f.read())
        else:
            # Needs to login
            self.redirect(users.create_login_url(self.request.uri))

class NotesRPC(webapp.RequestHandler):
    def get(self):
        user = users.get_current_user()
        jsonResult = []
        
        if user:
            # Retreiving notes
            query = Note.all()
            query.filter('user = ', user)
            notes = query.fetch(limit=MaximimNotesPerPage)
            for note in notes:
                jsonNote = {
                    'pk': note.key().id(),
                    'text': note.text,
                    'left': note.left,
                    'top': note.top,
                    'width': note.width,
                    'height': note.height
                }
                
                jsonResult.append(jsonNote)
            
        jsonResponseString = simplejson.dumps(jsonResult, separators=(',',':'))
        self.response.out.write(jsonResponseString)

    def post(self):
        user = users.get_current_user()
        jsonNotes = self.request.get('notes')
        
        if user and jsonNotes:
            notes = simplejson.loads(jsonNotes)
            for note in notes:
                # TODO: Validate the note
                
                pk = None
                if note.has_key('pk'):
                    pk = note['pk']
                
                if pk is not None:
                    logging.error("Has primary key: %s" % pk)
                    n = Note().get_by_id(int(pk))
                else:
                    logging.error("New note")
                    # This is a new note.
                    n = Note()
                    
                n.user = user
                n.text = note['text']
                n.left = int(note['left'])
                n.top = int(note['top'])
                n.width = int(note['width'])
                n.height = int(note['height'])
                n.put()
                
                