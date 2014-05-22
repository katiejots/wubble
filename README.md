wubble
================

# Running Locally

* Install PostgreSQL 9.2 or above 

* Change default database details in _src/Main.hs_ (127.0.0.1, 5432, postgres, '', postgres) to match your local database 

* Start your database

* Run the SQL from _bin/import.sql_  on your local database (the command might look something like this: `psql -U postgres -h 127.0.0.1 < bin/import.sql`)

* Install GHC 7.6 or above and Cabal 1.18 or above (you can use the commands `cabal update` and `cabal install cabal-install --global` to get the latest version)

* Change into the directory where you cloned this source code and run the following commands:

        cabal sandbox init
        cabal install --only-dependencies
 
* To run the app, issue the following command:

        cabal run 127.0.0.1 4000

* View the app at http://localhost:4000

 
# Running on OpenShift

* Create an [OpenShift Online account](https://www.openshift.com/app/account/new)

* Install the [RHC Client tools](https://www.openshift.com/developers/rhc-client-tools-install)

* Run the command `rhc setup` to configure RHC ready for use

* In a different directory to this source code, create a Haskell Scotty application with a PostgreSQL 9.2 database with the following command:

        app create scottyapp http://www.accursoft.com/cartridges/scotty.yml --from-code=http://github.com/codemiller/openshift-scotty.git

* Add a copy of the app source with the following command:

        git pull -s recursive -X theirs git://github.com/codemiller/wubble.git

* Import the SQL from _bin/import.sql_ into the OpenShift database with commands such as the following. You can use `rhc app show` to check the OpenShift database credentials.

         rhc port-forward 
         # Put the above into the background or open another terminal
         psql -h 127.0.0.1 -p 5432 -U adminabcabc1 -d wubble < bin/import.sql 
         # Replace port with the one shown in the port-forwarding output

* Push the new app code to OpenShift with the command `git push`

* View the app at the URL http://wubble-{yourdomain}.rhcloud.com; you can use the command `rhc app show -a wubble` to check the URL

