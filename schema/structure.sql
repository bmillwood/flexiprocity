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
  , facebook_id text UNIQUE NOT NULL
  , bio text NOT NULL DEFAULT ''
  , visible_to audience NOT NULL DEFAULT 'self'
  );
INSERT INTO users (facebook_id, visible_to)
VALUES ('_likes_me', 'everyone'), ('_stranger', 'everyone')
;

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

CREATE TABLE woulds
  ( would_id bigint PRIMARY KEY GENERATED ALWAYS AS IDENTITY
  , name text NOT NULL
  );
INSERT INTO woulds (name)
VALUES ('Hang out sometime'), ('Go on a date or something');
GRANT SELECT ON woulds TO api;

CREATE TABLE user_woulds
  ( user_id bigint  NOT NULL REFERENCES users(user_id)   ON DELETE CASCADE
  , would_id bigint NOT NULL REFERENCES woulds(would_id) ON DELETE CASCADE
  , with_id bigint  NOT NULL REFERENCES users(user_id)   ON DELETE CASCADE
  , PRIMARY KEY (user_id, would_id, with_id)
  );

CREATE POLICY only_my_woulds ON user_woulds FOR ALL TO api
  USING (user_id = current_user_id())
  WITH CHECK (user_id = current_user_id());
ALTER TABLE user_woulds ENABLE ROW LEVEL SECURITY;

GRANT SELECT, INSERT, DELETE ON user_woulds TO api;

CREATE VIEW user_profiles AS
  SELECT
    users.user_id
  , users.facebook_id
  , users.bio
  , users.user_id IN (
      SELECT friend_id
      FROM facebook_friends f
      WHERE f.user_id = current_user_id()
    ) AS is_friend
  , COALESCE(
      array_agg(uw.would_id) FILTER (WHERE uw.would_id IS NOT NULL)
    , '{}'
    ) AS you_would
  , COALESCE(
      array_agg(matches.would_id) FILTER (WHERE matches.would_id IS NOT NULL)
    , '{}'
    ) AS matched_woulds
  FROM users
  LEFT JOIN facebook_friends fwu
    ON users.user_id = fwu.user_id
   AND fwu.friend_id = current_user_id()
  LEFT JOIN user_woulds uw
    ON uw.user_id = current_user_id()
   AND uw.with_id = users.user_id
  LEFT JOIN user_woulds matches
    ON matches.user_id = users.user_id
   AND matches.would_id = uw.would_id
   AND matches.with_id = current_user_id()
  WHERE users.visible_to = 'everyone'
     OR (users.visible_to = 'friends'
        AND users.user_id IN (
          SELECT f.user_id
          FROM facebook_friends f
          WHERE f.friend_id = current_user_id()
        ))
     OR users.user_id = current_user_id()
     OR users.user_id IN (
          SELECT w.user_id
          FROM user_woulds w
          WHERE w.user_id = users.user_id
            AND w.with_id = current_user_id()
        )
  GROUP BY users.user_id;
GRANT SELECT ON user_profiles TO api;
