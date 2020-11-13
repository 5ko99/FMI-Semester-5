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

getDisciplineName :: Discipline
getDisciplineName (name, _, _) = name

getDisciplineID :: Discipline
getDisciplineID (_, id, _) = id

getStudentFn :: Student -> Fn
getStudentFn (_, _, fn, _) = fn

getStudentName :: Student -> FirstName
getStudentName (name, _, _, _) = name

getStudentLastName :: Student -> LastName
getStudentLastName (_, lastName, _, _) = lastName

getDisciplineId :: StudentScore -> DisciplineID
getDisciplineId (_, _, id) = id

getScoreFn :: StudentScore -> Fn
getScoreFn (fn, _, _) = fn

getScore :: StudentScore -> Score
getScore (_, score, _) = score

getNamesByDisciplineIDHelper :: [StudentScore] -> [Student] -> DisciplineID -> [(FirstName, LastName)] -> [(FirstName, LastName)]
getNamesByDisciplineIDHelper _ [] _ res = res
getNamesByDisciplineIDHelper [] _ _ _ = []
getNamesByDisciplineIDHelper studentScore students disciplineID res =
  let student = head students in getNamesByDisciplineIDHelper studentScore (tail students) disciplineID (helper student studentScore []) ++ res
  where
    helper :: Student -> [StudentScore] -> [(FirstName, LastName)] -> [(FirstName, LastName)]
    helper curStudent score res
      | null score = res
      | getScoreFn (head score) == getStudentFn curStudent && getDisciplineId (head score) == disciplineID =
        helper curStudent (tail score) $ (getStudentName curStudent, getStudentLastName curStudent) : res
      | otherwise = helper curStudent (tail score) res

getNamesByDisciplineID :: [StudentScore] -> [Student] -> DisciplineID -> [(FirstName, LastName)]
getNamesByDisciplineID studentScore students disciplineID =
  getNamesByDisciplineIDHelper studentScore students disciplineID []

a :: [StudentScore]
a = [("46", 6, "1"), ("45", 4, "1"), ("46", 4, "2"), ("44", 5, "1"), ("45", 2, "2"), ("44", 3, "2"), ("44", 5, "3"), ("46", 6, "4"), ("44", 4, "5"), ("45", 5, "5")]

b :: [Student]
b = [("Petko", "Kamenov", "46", 3), ("Alex", "Turner", "45", 2), ("John", "Lennon", "44", 4)]

c :: DisciplineID
c = "5"

getResultsHelper :: [StudentScore] -> [Discipline] -> [(DisciplineName, DisciplineID, Score)] -> [(DisciplineName, DisciplineID, Score)]
getResultsHelper studentsScores disciplines res =
  let discipline = head disciplines
   in getResultsHelper
        studentsScores
        (tail disciplines)
        (getDisciplineName discipline, getDisciplineId discipline, (helper studentsScores discipline 0 [])) :
      res
  where
    helper :: [StudentScore] -> Discipline -> Int -> Float
    helper studentsScores curDisc count res
      | null studentsScores = res
      | getDisciplineId curDisc == getDisciplineId (head studentsScores) =
        helper (tail studentsScores) curDisc (count + 1) (res + getScore (head studentsScores)) / (count + 1)
