(ns
    org.sg.discord-lib.core
    (:require
     [discljord.snowflakes
      :as
      snowf]))

(defn
  msg-ref
  [e
   {:keys [channel-id guild-id id]}]
  (assoc
   e
   :message-reference
   {:channel_id channel-id
    :guild_id guild-id
    :message_id id}))


(defn
  snowflake->age-ms
  [id]
  (some->
   id
   snowf/timestamp
   (- (System/currentTimeMillis))
   Math/abs))
