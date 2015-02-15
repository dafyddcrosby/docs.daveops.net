------------------------
Ruby on Rails - Omniauth
------------------------
@[[Ruby on Rails]] Twitter Facebook Linkedin 


Installing
==============================
These notes are assuming you're also allowing regular email/password logins. It's greatly simplified if you don't...
{{{
rails generate model Authorization provider:string uid:string user_id:integer
}}}
Gemfile
-----------------------------------
{{{
gem 'omniauth'
gem 'omniauth-twitter'
gem 'omniauth-facebook'
gem 'omniauth-linkedin'
}}}

config/initializers/omniauth.rb
-----------------------------------

.. code-block:: ruby

 Rails.application.config.middleware.use OmniAuth::Builder do
   provider :twitter, 'CONSUMER_KEY', 'CONSUMER_SECRET'
   provider :facebook, 'APP_ID', 'APP_SECRET'
   provider :linked_in, 'CONSUMER_KEY', 'CONSUMER_SECRET'
 end

app/models/user.rb
==============================
add:

.. code-block:: ruby

 has_many :authorizations
 
 def add_provider(auth_hash)
   unless authorizations.find_by_provider_and_uid(auth_hash["provider"], auth_hash["uid"])
     Authorization.create :user => self, :provider => auth_hash["provider"], :uid => auth_hash["uid"]
   end
 end

app/controllers/sessions_controller.rb
======================================
.. code-block:: ruby

 def omniauth_create
   auth_hash = request.env['omniauth.auth']
   if session[:user_id]
     # Means user is signed in. Add the authorization to the user
     user = User.find(session[:user_id])
     user.add_provider(auth_hash)
   else
     auth = Authorization.find_or_create(auth_hash)
     # Create the session
     session[:user_id] = auth.user_id
     user = User.find(session[:user_id])
   end
   sign_in user
   redirect_back_or user
 end

app/models/authorization.rb
==============================
.. code-block:: ruby

 belongs_to :user
 validates :provider, :uid, :presence => true
 
 def self.find_or_create(auth_hash)
   unless auth = find_by_provider_and_uid(auth_hash["provider"], auth_hash["uid"])
     user = User.find_by_email(auth_hash["info"]["email"])
     if not user
       # If it's a new user, we want to give them a solid password
       random_string = SecureRandom.base64(30)
       user = User.create :name => auth_hash["info"]["name"],
                           :email => auth_hash["info"]["email"],
                           :password => random_string,
                           :password_confirmation => random_string
     end
     auth = create :user_id => user, :provider => auth_hash["provider"], :uid => auth_hash["uid"]
   end
   auth
 end

config/routes.rb
==============================
.. code-block:: ruby

 match '/auth/:provider/callback', to: 'sessions#create'
 match '/auth/failure', to: 'sessions#failure'

