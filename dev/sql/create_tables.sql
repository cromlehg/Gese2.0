CREATE TABLE roles (
  user_id                   BIGINT UNSIGNED NOT NULL,
  role                      INT UNSIGNED NOT NULL,
  PRIMARY KEY (user_id, role)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci;

CREATE TABLE currencies (
  id                        SERIAL PRIMARY KEY,
  name                      VARCHAR(100) NOT NULL,
  ticker                    VARCHAR(50) NOT NULL
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci;  

CREATE TABLE balances (
  id                        SERIAL PRIMARY KEY,
  owner_id                  BIGINT UNSIGNED NOT NULL,
  owner_type                INT UNSIGNED NOT NULL,
  currency_id               INT UNSIGNED NOT NULL,
  updated                   BIGINT UNSIGNED NOT NULL,
  balance_type              INT UNSIGNED NOT NULL,
  `value`                   INT UNSIGNED NOT NULL
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci;  

CREATE TABLE comments (
  id                        SERIAL PRIMARY KEY,
  post_id                   BIGINT UNSIGNED NOT NULL,
  owner_id                  BIGINT UNSIGNED NOT NULL,
  parent_id                 BIGINT UNSIGNED,
  content                   TEXT NOT NULL,
  content_type              INT UNSIGNED NOT NULL,
  created                   BIGINT UNSIGNED NOT NULL,
  likes_count               INT UNSIGNED NOT NULL,
  reward_token              BIGINT UNSIGNED NOT NULL,
  reward_power              BIGINT UNSIGNED NOT NULL,
  reward_dollar             BIGINT UNSIGNED NOT NULL,
  status                    INT UNSIGNED NOT NULL
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci;

CREATE TABLE sessions (
  id                        SERIAL PRIMARY KEY,
  user_id                   BIGINT UNSIGNED NOT NULL,
  ip                        VARCHAR(100) NOT NULL, 
  session_key               VARCHAR(100) NOT NULL UNIQUE,
  created                   BIGINT UNSIGNED NOT NULL,
  expire                    BIGINT UNSIGNED NOT NULL
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci;

CREATE TABLE accounts (
  id                        SERIAL PRIMARY KEY,
  login                     VARCHAR(100) NOT NULL UNIQUE,
  email                     VARCHAR(100) NOT NULL UNIQUE,
  hash                      VARCHAR(60),
  avatar                    VARCHAR(255),
  background                VARCHAR(255),
  user_status               INT UNSIGNED NOT NULL,
  account_status            INT UNSIGNED NOT NULL,
  name                      VARCHAR(100),
  surname                   VARCHAR(100),
  platform_eth              VARCHAR(100) UNIQUE,
  timezone_id               INT UNSIGNED NOT NULL,
  registered                BIGINT UNSIGNED NOT NULL,
  confirm_code              VARCHAR(100),
  posts_counter             INT UNSIGNED NOT NULL,
  posts_counter_started     BIGINT UNSIGNED NOT NULL,
  likes_counter             INT UNSIGNED NOT NULL,
  likes_counter_started     BIGINT UNSIGNED NOT NULL,
  comments_counter          INT UNSIGNED NOT NULL,
  comments_counter_started  BIGINT UNSIGNED NOT NULL,
  posts_count               BIGINT UNSIGNED NOT NULL,
  about                     VARCHAR(255),
  is_show_brand             BOOLEAN NOT NULL,
  ITN                       VARCHAR(100),
  IEC                       VARCHAR(100),
  phones                    VARCHAR(100),
  gender                    VARCHAR(20),
  age                       INT UNSIGNED,
  user_type                 INT UNSIGNED NOT NULL,
  rate                      INT UNSIGNED NOT NULL,
  sex                       BIGINT UNSIGNED,
  education                 VARCHAR(255),
  birthday                  BIGINT UNSIGNED,
  bought                    INT UNSIGNED NOT NULL,
  rate_count                INT UNSIGNED NOT NULL
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci;

CREATE TABLE posts (
  id                        SERIAL PRIMARY KEY,
  owner_id                  BIGINT UNSIGNED NOT NULL,
  product_id                BIGINT UNSIGNED,
  title                     VARCHAR(100) NOT NULL,
  thumbnail                 VARCHAR(255),
  content                   TEXT NOT NULL,
  content_type              INT UNSIGNED NOT NULL,
  post_type                 INT UNSIGNED NOT NULL,
  balance                   BIGINT UNSIGNED,
  `limit`                   BIGINT UNSIGNED,
  status                    INT UNSIGNED NOT NULL,
  promo                     BIGINT UNSIGNED NOT NULL,
  alcohol                   INT UNSIGNED,
  `value`                   INT UNSIGNED,
  address                   TEXT,
  type_status               INT UNSIGNED NOT NULL,
  likes_count               INT UNSIGNED NOT NULL,
  comments_count            INT UNSIGNED NOT NULL,
  reviews_count             INT UNSIGNED,
  created                   BIGINT UNSIGNED NOT NULL,
  views_count               INT UNSIGNED NOT NULL,
  reward_type               INT UNSIGNED NOT NULL,
  reward_token              BIGINT UNSIGNED NOT NULL,
  reward_power              BIGINT UNSIGNED NOT NULL,
  reward_dollar             BIGINT UNSIGNED NOT NULL,
  rate                      BIGINT UNSIGNED NOT NULL,
  rate_count                BIGINT UNSIGNED NOT NULL,
  moderate_status           INT UNSIGNED NOT NULL
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci;

CREATE TABLE likes (
  id                        SERIAL PRIMARY KEY,
  owner_id                  BIGINT UNSIGNED NOT NULL,
  target_type               INT UNSIGNED NOT NULL,
  target_id                 BIGINT UNSIGNED NOT NULL,
  created                   BIGINT UNSIGNED NOT NULL
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci;

CREATE TABLE txs (
  id                        SERIAL PRIMARY KEY,
  created                   BIGINT UNSIGNED NOT NULL,
  scheduled                 BIGINT UNSIGNED,
  processed                 BIGINT UNSIGNED,
  `from_type`               INT UNSIGNED NOT NULL,
  `to_type`                 INT UNSIGNED NOT NULL,
  `from_id`                 BIGINT UNSIGNED,
  `to_id`                   BIGINT UNSIGNED,
  `from_route_type`         INT UNSIGNED,
  `to_route_type`           INT UNSIGNED,
  `from_route_id`           BIGINT UNSIGNED,
  `to_route_id`             BIGINT UNSIGNED,
  `from`                    VARCHAR(100),
  `to`                      VARCHAR(100),
  `type`                    INT UNSIGNED NOT NULL,
  `msg`                     VARCHAR(100),
  state                     INT UNSIGNED NOT NULL,
  currency_id               INT UNSIGNED NOT NULL,
  amount                    BIGINT UNSIGNED NOT NULL
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci;

CREATE TABLE scheduled_tasks (
  id                        SERIAL PRIMARY KEY,
  executed                  BIGINT UNSIGNED,
  task_type                 INT UNSIGNED NOT NULL,
  planned                   BIGINT UNSIGNED,
  account_id                BIGINT UNSIGNED,
  product_id                BIGINT UNSIGNED
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci;

CREATE TABLE tags (
  id                        SERIAL PRIMARY KEY,
  name                      VARCHAR(100) NOT NULL UNIQUE
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci;

CREATE TABLE tags_to_targets (
  target_id                 BIGINT UNSIGNED NOT NULL,
  tag_id                    BIGINT UNSIGNED NOT NULL,
  target_type               INT UNSIGNED NOT NULL,
  created                   BIGINT UNSIGNED NOT NULL,
  PRIMARY KEY (target_id, tag_id, target_type)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci;

CREATE TABLE `batches` (
  id                        SERIAL PRIMARY KEY,
  product_id                BIGINT UNSIGNED NOT NULL,
  created                   BIGINT UNSIGNED NOT NULL,
  count                     INT UNSIGNED NOT NULL,
  price                     BIGINT UNSIGNED NOT NULL
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci;  

CREATE TABLE `positions` (
  id                        SERIAL PRIMARY KEY,
  item_id                   BIGINT UNSIGNED NOT NULL,
  `timestamp`               BIGINT UNSIGNED NOT NULL,
  longitude                 DOUBLE NOT NULL,
  latitude                  DOUBLE NOT NULL,
  accuracy                  DOUBLE NOT NULL
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci;  

CREATE TABLE `items` (
  id                        SERIAL PRIMARY KEY,
  batch_id                  BIGINT UNSIGNED NOT NULL,
  code                      VARCHAR(100),
  status                    INT UNSIGNED,
  bought                    BIGINT UNSIGNED,
  buyer_id                  BIGINT UNSIGNED
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci;  

CREATE TABLE `marketing_campaigns` (
  id                        SERIAL PRIMARY KEY,
  product_id                BIGINT UNSIGNED NOT NULL,
  initial_count             INT UNSIGNED,
  count                     INT UNSIGNED,
  price                     Long,
  start                     BIGINT UNSIGNED NOT NULL,
  `end`                     BIGINT UNSIGNED NOT NULL,
  status                    INT UNSIGNED,
  descr                     TEXT,
  title                     VARCHAR(255) NOT NULL
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci; 

CREATE TABLE wallets (
  id                        SERIAL PRIMARY KEY,
  owner_id                  BIGINT UNSIGNED,
  owner_type_id             INT UNSIGNED NOT NULL,
  base_currency_id          INT UNSIGNED NOT NULL,
  address                   VARCHAR(100) NOT NULL,
  private_key               VARCHAR(100) NOT NULL
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci;  

CREATE TABLE short_options (
  id                        SERIAL PRIMARY KEY,
  name                      VARCHAR(100) NOT NULL,
  descr                     VARCHAR(255) NOT NULL,
  `type`                    VARCHAR(100) NOT NULL,
  `value`                   VARCHAR(100) NOT NULL
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci;  
