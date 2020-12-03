require 'plist'

class TestController < ApplicationController
    skip_before_filter :verify_authenticity_token
    def index
        begin
            p = Plist::parse_xml(request.body.read)
        
            sum = 0
            p['ValuesToSum'].each do |value|
                sum += value
            end
        
            render :xml => { "code" => "ok", "result" => sum, "pid" => Process.pid }.to_plist
        rescue
            render :xml => { "code" => "fail" }.to_plist
        end
    end
end
