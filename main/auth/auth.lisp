(in-package :rdf)

(defmacro setup-auth (fields &key auth-types override)
  "Setup the RDF automatic authorisation system. This adds the 'rdf:user-auth'
  and 'rdf:user-info' tables, according to the given spec. These entities can be
  accessed with the exported rdf:user-info and rdf:user-auth symbols.

  The user-auth entity contains data used to authorise the user - email and
  password, for example.

  The user-info entity contains additional data, specified in the user-spec.

  The user-auth entity is a 'child' of the user-info entity. In general, any
  data 'owned' by the user should be a child of the 'user-info' entity.

  These being separate entities is useful, as when displaying a user's 'profile'
  you can just select the user-info entity, rather than the user-auth entity and
  have to manually remove the password hash from the returned entity for
  security purposes.

  If no additional data was specified, the user-info entity is still created,
  but it only contains an ID.


  # Params
  - override
    The override &key parameter will delete the old user-info and user-auth tables
    if it's T, and won't if NIL.

  - auth-types
    This is a list of authorization types. Supported types:
    - :email-password
    At least 1 auth type should be specified. This parameter is currently only
    here to provide API flexibility.
  - fields
    This is a list of additional fields associated with each user. This is the
    stuff you'd put on a registration form. This list matches the format of the
    slot definitions for defentity.
  # example
  This will setup the auth system to accept email / password as the
  authorization method. Each user will have a first name and a last name
  associated with them.
  (setup-auth ((first-name \"VARCHAR(256)\")
    (last-name \"VARCHAR(256)\"))
    :auth-types (:email-password)
  )
  "
  (if (not auth-types)
      (error "Need to provide at least 1 :auth-type when calling setup-auth"))
  (if (not (eq :email-password (car auth-types)))
      (error "Unknown auth type ~a. Supported:
- :email-password" (car auth-types)))
  (if (> (length auth-types) 1) (error "WARNING: setup-auth only supports 1
  authorization type, you entered two."))
  `(progn
     (defentity user-info ,fields :override ,override)
     ;; Just assume email-password auth for the moment
     (defentity user-auth
         ((email "VARCHAR(256)" :not-null :unique) (pass "CHAR(116)" :not-null))
       :parents (user-info) :override ,override)))
