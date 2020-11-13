type Fn = String -- факултетен номер на студент

type FirstName = String -- първо име на студент

type LastName = String -- последно име на студент

type Score = Float -- оценка

type DisciplineID = String -- идентификационен номер на дисциплина

type Course = Integer -- курс на студент

type DisciplineName = String -- име на съотдетна дисциплина

type CourseYear = Integer -- година от обучението за която е подходяща съответна дисциплина

type StudentScore = (Fn, Score, DisciplineID)

type Student = (FirstName, LastName, Fn, Course)

type Discipline = (DisciplineName, DisciplineID, CourseYear)

-- getNamesByDisciplineID :: [StudentScore] -> [Student] -> DisciplineID -> (FirstName, LastName)
-- getNamesByDisciplineID studentScore student disciplineID =
--     let student = head Student in

main = do
  a <- getLine
  b <- getLine
  putStr (a ++ b ++ "\n")