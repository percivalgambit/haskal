module HaskalCommon where

type FrequencyTuple = (String,[(Int,Int)])
type ProcessedModel = [FrequencyTuple]

serializedFile :: FilePath
serializedFile = "sokal.model"
