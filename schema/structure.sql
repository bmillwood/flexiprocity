GRANT CONNECT ON DATABASE flexiprocity TO api;

CREATE FUNCTION get_facebook_id() RETURNS text
  LANGUAGE sql SECURITY INVOKER STABLE PARALLEL RESTRICTED
  BEGIN ATOMIC
    SELECT current_setting('jwt.claims.facebookUserId', true);
  END;

CREATE TYPE audience AS ENUM
  ( 'self'
  , 'friends'
  , 'everyone'
  );

-- Postgraphile doesn't support procedures
CREATE TYPE unit AS ENUM ('unit');

CREATE TABLE users
  ( user_id bigint PRIMARY KEY GENERATED ALWAYS AS IDENTITY
  , facebook_id text UNIQUE
  , name text
  , bio text NOT NULL DEFAULT ''
  , visible_to audience NOT NULL DEFAULT 'self'
  , fbid_visible_to audience
  );

CREATE FUNCTION current_user_id() RETURNS bigint
  LANGUAGE sql SECURITY DEFINER STABLE PARALLEL RESTRICTED
  BEGIN ATOMIC
    SELECT user_id FROM users
    WHERE facebook_id = get_facebook_id();
  END;
REVOKE EXECUTE ON FUNCTION current_user_id FROM public;
GRANT  EXECUTE ON FUNCTION current_user_id TO api;

CREATE FUNCTION my_user() RETURNS users
  LANGUAGE sql SECURITY DEFINER STABLE PARALLEL RESTRICTED
  BEGIN ATOMIC
    SELECT * FROM users WHERE user_id = current_user_id();
  END;
REVOKE EXECUTE ON FUNCTION my_user FROM public;
GRANT  EXECUTE ON FUNCTION my_user TO api;

CREATE FUNCTION update_me(bio text, visible_to audience) RETURNS users
  LANGUAGE sql SECURITY DEFINER VOLATILE PARALLEL RESTRICTED
  BEGIN ATOMIC
    UPDATE users
    SET bio = COALESCE(update_me.bio, users.bio)
    , visible_to = COALESCE(update_me.visible_to, users.visible_to)
    WHERE user_id = current_user_id()
    RETURNING *;
  END;
REVOKE EXECUTE ON FUNCTION update_me FROM public;
GRANT  EXECUTE ON FUNCTION update_me TO api;

CREATE FUNCTION get_or_create_user_id() RETURNS bigint
  LANGUAGE sql SECURITY DEFINER VOLATILE PARALLEL RESTRICTED
  BEGIN ATOMIC
    WITH jwt AS (
      SELECT get_facebook_id() AS facebook_id
    )
    , created AS (
      INSERT INTO users (facebook_id)
      SELECT facebook_id FROM jwt WHERE jwt.facebook_id IS NOT NULL
      EXCEPT
      SELECT facebook_id FROM users
      RETURNING user_id
    )
    SELECT user_id FROM users
    JOIN jwt USING (facebook_id)
    UNION ALL
    SELECT user_id FROM created;
  END;
REVOKE EXECUTE ON FUNCTION get_or_create_user_id FROM public;
GRANT  EXECUTE ON FUNCTION get_or_create_user_id TO api;

CREATE TABLE facebook_friends
  ( user_id   bigint REFERENCES users(user_id) ON DELETE CASCADE
  , friend_id bigint REFERENCES users(user_id) ON DELETE CASCADE
  , PRIMARY KEY (user_id, friend_id)
  );

CREATE FUNCTION set_facebook_friends(friend_fbids text[]) RETURNS unit
  LANGUAGE sql SECURITY DEFINER
  BEGIN ATOMIC
    WITH friend_ids AS (
      SELECT user_id AS friend_id FROM users WHERE facebook_id = ANY(friend_fbids)
    )
    , deleted AS (
      DELETE FROM facebook_friends
      WHERE user_id = current_user_id()
      AND friend_id <> ALL(SELECT friend_id FROM friend_ids)
    )
    , inserted AS (
      INSERT INTO facebook_friends (user_id, friend_id)
      SELECT current_user_id(), friend_id
      FROM friend_ids
      ON CONFLICT DO NOTHING
    )
    SELECT 'unit'::unit;
  END;
REVOKE EXECUTE ON FUNCTION set_facebook_friends FROM public;
GRANT  EXECUTE ON FUNCTION set_facebook_friends TO api;

CREATE VIEW user_profiles AS
  SELECT
    users.user_id
  , CASE
      WHEN users.fbid_visible_to = 'everyone'
        OR (users.fbid_visible_to = 'friends' AND fr IS NOT NULL)
        OR users.user_id = current_user_id()
        THEN users.facebook_id
      ELSE NULL
    END AS facebook_id
  , users.name
  , users.bio
  FROM users
  LEFT JOIN facebook_friends fr
    ON users.user_id = fr.user_id AND fr.friend_id = current_user_id()
  WHERE users.visible_to = 'everyone'
     OR (users.visible_to = 'friends' AND fr IS NOT NULL)
     OR users.user_id = current_user_id();
GRANT SELECT ON user_profiles TO api;
