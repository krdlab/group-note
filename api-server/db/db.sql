-------------------------------------------------
-- DON'T USE THIS SCHEMA! THIS IS A EXPERIMENTAL.
-------------------------------------------------

DROP TABLE IF EXISTS `user`;
CREATE TABLE IF NOT EXISTS `user` (
  `id`          INTEGER         NOT NULL PRIMARY KEY AUTOINCREMENT,
  `id_name`     VARCHAR(64)     NOT NULL,
  `issuer`      VARCHAR(256)    NOT NULL,
  `sub`         VARCHAR(128)    NOT NULL,
  `name`        VARCHAR(128)    NOT NULL,
  `email`       VARCHAR(256)    NOT NULL,
  `created_at`  DATETIME        NOT NULL,
  CONSTRAINT `user_UNIQUE` UNIQUE (`id_name`),
  CONSTRAINT `user_openid_UNIQUE` UNIQUE (`issuer`, `sub`)
);

DROP TABLE IF EXISTS `team`;
CREATE TABLE IF NOT EXISTS `team` (
  `id`          INTEGER     NOT NULL PRIMARY KEY AUTOINCREMENT,
  `id_name`     VARCHAR(64) NOT NULL,
  `name`        VARCHAR(64) NOT NULL,
  `owner_id`    INTEGER     NOT NULL,
  `created_at`  DATETIME    NOT NULL,
  `updated_at`  DATETIME    NOT NULL,
  `version`     INTEGER     NOT NULL,
  CONSTRAINT `team_UNIQUE` UNIQUE (`id_name`),
  CONSTRAINT `fk_team_user` FOREIGN KEY (`owner_id`) REFERENCES `user` (`id`)
);
CREATE INDEX `fk_team_user_idx` ON `team` (`owner_id`);

DROP TABLE IF EXISTS `member`;
CREATE TABLE IF NOT EXISTS `member` (
  `id`          INTEGER     NOT NULL PRIMARY KEY AUTOINCREMENT,
  `team_id`    INTEGER     NOT NULL,
  `user_id`     INTEGER     NOT NULL,
  `created_at`  DATETIME    NOT NULL,
  CONSTRAINT `member_UNIQUE` UNIQUE (`team_id`, `user_id`),
  CONSTRAINT `fk_member_team` FOREIGN KEY (`team_id`) REFERENCES `team` (`id`),
  CONSTRAINT `fk_member_user` FOREIGN KEY (`user_id`) REFERENCES `user` (`id`)
);
CREATE INDEX `fk_member_team_idx` ON `member` (`team_id`);
CREATE INDEX `fk_member_user_idx` ON `member` (`user_id`);

DROP TABLE IF EXISTS `invite`;
CREATE TABLE IF NOT EXISTS `invite` (
  `id`          INTEGER     NOT NULL PRIMARY KEY AUTOINCREMENT,
  `code`        VARCHAR(32) NOT NULL,
  `team_id`    INTEGER     NOT NULL,
  CONSTRAINT `invite_UNIQUE` UNIQUE (`code`),
  CONSTRAINT `fk_invite_team` FOREIGN KEY (`team_id`) REFERENCES `team` (`id`)
);
CREATE INDEX `fk_invite_team_idx` ON `invite` (`team_id`);

DROP TABLE IF EXISTS `session`;
CREATE TABLE IF NOT EXISTS `session` (
  `id`              INTEGER     NOT NULL PRIMARY KEY AUTOINCREMENT,
  `token`           VARCHAR(64) NOT NULL,
  `registration`    INTEGER(1)  NOT NULL, -- BOOLEAN
  `created_at`      DATETIME    NOT NULL,
  CONSTRAINT `session_UNIQUE` UNIQUE (`token`)
);

DROP TABLE IF EXISTS `session_state`;
CREATE TABLE IF NOT EXISTS `session_state` (
  `session_id`  INTEGER     NOT NULL PRIMARY KEY,
  `token`       VARCHAR(64) NOT NULL,
  CONSTRAINT `session_state_UNIQUE` UNIQUE (`token`),
  CONSTRAINT `fk_sessoin_state_sessoin` FOREIGN KEY (`session_id`) REFERENCES `session` (`id`)
);

DROP TABLE IF EXISTS `session_with_invite`;
CREATE TABLE IF NOT EXISTS `session_with_invite` (
  `session_id`  INTEGER NOT NULL PRIMARY KEY,
  `invite_id`   INTEGER NOT NULL,
  CONSTRAINT `fk_session_with_invite_session` FOREIGN KEY (`session_id`) REFERENCES `session` (`id`),
  CONSTRAINT `fk_session_with_invite_invite` FOREIGN KEY (`invite_id`) REFERENCES `invite` (`id`)
);
CREATE INDEX `fk_session_with_invite_invite_idx` ON `session_with_invite` (`invite_id`);

DROP TABLE IF EXISTS `user_session`;
CREATE TABLE IF NOT EXISTS `user_session` (
  `session_id`  INTEGER NOT NULL PRIMARY KEY,
  `user_id`     INTEGER NOT NULL,
  CONSTRAINT `fk_user_session_session` FOREIGN KEY (`session_id`) REFERENCES `session` (`id`),
  CONSTRAINT `fk_user_session_user` FOREIGN KEY (`user_id`) REFERENCES `user` (`id`)
);
CREATE INDEX `fk_user_session_user_idx` ON `user_session` (`user_id`);

DROP TABLE IF EXISTS `authorization`;
CREATE TABLE IF NOT EXISTS `authorization` (
  `session_id`      INTEGER     NOT NULL PRIMARY KEY,
  `access_token`    VARCHAR(64) NOT NULL,
  `user_id`         INTEGER     NOT NULL,
  `created_at`      DATETIME    NOT NULL,
  CONSTRAINT `authorization_UNIQUE` UNIQUE (`access_token`),
  CONSTRAINT `fk_authorization_session` FOREIGN KEY (`session_id`) REFERENCES `sessoin` (`id`),
  CONSTRAINT `fk_authorization_user` FOREIGN KEY (`user_id`) REFERENCES `user` (`id`)
);
CREATE INDEX `fk_authorization_user_idx` ON `authorization` (`user_id`);

DROP TABLE IF EXISTS `note`;
CREATE TABLE IF NOT EXISTS `note` (
  `id`          INTEGER     NOT NULL PRIMARY KEY AUTOINCREMENT,
  `id_name`     VARCHAR(64) NOT NULL,
  `team_id`    INTEGER     NOT NULL,
  `member_id`   INTEGER     NOT NULL,
  `title`       TEXT        NOT NULL,
  `content`     MEDIUMTEXT  NOT NULL,
  `created_at`  DATETIME    NOT NULL,
  `updated_at`  DATETIME    NOT NULL,
  `version`     INTEGER     NOT NULL,
  CONSTRAINT `note_UNIQUE` UNIQUE (`id_name`),
  CONSTRAINT `fk_note_team` FOREIGN KEY (`team_id`) REFERENCES `team` (`id`),
  CONSTRAINT `fk_note_member` FOREIGN KEY (`member_id`) REFERENCES `member` (`id`)
);
CREATE INDEX `fk_note_team_idx` ON `note` (`team_id`);
CREATE INDEX `fk_note_member_idx` ON `note` (`member_id`);
