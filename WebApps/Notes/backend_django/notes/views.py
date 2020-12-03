from django.http import HttpResponse
from django.contrib.auth.decorators import login_required
from django.shortcuts import render_to_response
from django.core import serializers
from backend_django.notes.models import Note

def json_notes(request):
    if request.method == 'POST':
        jsonNotes = request.POST['notes']
        
        for deserialized_object in serializers.deserialize('json', jsonNotes):
            # Don't trust the web request to have the user right. Besides it is known since
            # this request requires a login.
            deserialized_object.object.user = request.user 
            deserialized_object.save()
        
        return HttpResponse("")
    else:
        users_notes = Note.objects.filter(user=request.user)
        json_data = serializers.serialize('json', users_notes)
        return HttpResponse(json_data)
    
json_notes = login_required(json_notes)    

