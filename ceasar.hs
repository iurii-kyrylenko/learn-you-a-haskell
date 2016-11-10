enc' sft = map $ chr . (+sft) . ord
dec' = enc' . negate

id' sft s = enc' sft . dec' sft $ s
id' sft = (enc' sft) . (dec' sft)
