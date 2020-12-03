import notes.views

from google.appengine.api import users
from google.appengine.ext import webapp
from google.appengine.ext.webapp.util import run_wsgi_app

class MainPage(webapp.RequestHandler):
    def get(self):
        user = users.get_current_user()
        if user:
            self.redirect("/notes")
        else:
            # TODO: Replace with nice home page with my own login fields, if possible.
            self.redirect(users.create_login_url(self.request.uri))
            

application = webapp.WSGIApplication(
    [
        ('/', MainPage),
        ('/notes', notes.views.NotesPage),
        ('/notes_rpc', notes.views.NotesRPC)
    ],
    debug=True)

def main():
    run_wsgi_app(application)
    
if __name__ == "__main__":
    main()
    
