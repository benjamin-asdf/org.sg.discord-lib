(ns org.sg.discord-lib.core)

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
