module HaskalCommon where

type FrequencyTuple = (String,[(Int,Int)])
type ProcessedModel = [FrequencyTuple]

-- Name of the file used to store the serialized processed model
serializedFile :: FilePath
serializedFile = "sokal.model"
