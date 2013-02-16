from django.conf.urls.defaults import *

urlpatterns = patterns('backend_django.notes.views',
    (r'^json_notes', 'json_notes'),
)
