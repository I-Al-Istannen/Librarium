### About

This is a small helper for our home library. It keeps track of your books, can
search them by title, isbn, summary or location.

### Location tracking
To track the location it just keeps track of and exposes a location as a string
you can freely set. As a small convenience it allows you to select any location
you have entered previously so you don't have to retype them every time.

### Running it

#### Frontend
1. Adjust the base url in the `.env.production` file
2. `yarn build`
3. Serve the static files in the `dist` directory *on all paths* (i.e. serve
   the index.html even if people request `/home`)

#### Backend
1. `stack build backend` (and then find the path in .stack-work) or `stack install backend`
2. `backend-exe <data directory> <username> <password>`

### Screenshots
![Image](https://i.imgur.com/ggeAgl9.png)
