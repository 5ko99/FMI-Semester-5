import Data.Maybe

type Fn = String -- факултетен номер на студент

type FirstName = String -- първо име на студент

type LastName = String -- последно име на студент

type Score = Float -- оценка

type DisciplineID = String -- идентификационен номер на дисциплина

type Course = Integer -- курс на студент

type DisciplineName = String -- име на съответна дисциплина

type CourseYear = Integer -- година от обучението за която е подходяща съответна дисциплина

type StudentScore = (Fn, Score, DisciplineID)

type Student = (FirstName, LastName, Fn, Course)

type Discipline = (DisciplineName, DisciplineID, CourseYear)

--Getters
getDisciplineName :: Discipline -> DisciplineName
getDisciplineName (name, _, _) = name

getDisciplineID :: Discipline -> DisciplineID
getDisciplineID (_, id, _) = id

getStudentFn :: Student -> Fn
getStudentFn (_, _, fn, _) = fn

getStudentName :: Student -> FirstName
getStudentName (name, _, _, _) = name

getStudentLastName :: Student -> LastName
getStudentLastName (_, lastName, _, _) = lastName

getSCDisciplineId :: StudentScore -> DisciplineID
getSCDisciplineId (_, _, id) = id

getScoreFn :: StudentScore -> Fn
getScoreFn (fn, _, _) = fn

getScore :: StudentScore -> Score
getScore (_, score, _) = score

getDisciplineYear :: Discipline -> CourseYear
getDisciplineYear (_, _, discYear) = discYear

getStudentCourse :: Student -> Course
getStudentCourse (_, _, _, course) = course

--Main functions
getNamesByDisciplineID :: [StudentScore] -> [Student] -> DisciplineID -> [(FirstName, LastName)]
getNamesByDisciplineID studentScore students disciplineID = catMaybes $ [helper student studentScore | student <- students]
  where
    helper :: Student -> [StudentScore] -> Maybe (FirstName, LastName)
    helper student score
      | null score = Nothing
      | getScoreFn curScore == getStudentFn student && getSCDisciplineId curScore == disciplineID =
        Just (getStudentName student, getStudentLastName student)
      | otherwise = helper student (tail score)
      where
        curScore = head score

getResults :: [StudentScore] -> [Discipline] -> [(DisciplineName, DisciplineID, Score)]
getResults studentsScores disciplines = catMaybes $ [helper discipline studentsScores 0 0 | discipline <- disciplines]
  where
    helper :: Discipline -> [StudentScore] -> Int -> Score -> Maybe (DisciplineName, DisciplineID, Score)
    helper _ [] 0 _ = Nothing
    helper disc [] n res = let name = getDisciplineName disc; id = getDisciplineID disc in Just (name, id, res / fromIntegral n)
    helper disc stScores n res =
      if getDisciplineID disc == getSCDisciplineId curStScore
        then helper disc (tail stScores) (n + 1) (res + grade)
        else helper disc (tail stScores) n res
      where
        curStScore = head stScores
        grade = getScore curStScore

--last
getAboveAverageStudents :: [StudentScore] -> [Student] -> [Discipline] -> [Maybe Student] -> [Maybe Student]
getAboveAverageStudents _ [] _ res = res
getAboveAverageStudents studentScores students disciplines res  =
    getAboveAverageStudents studentScores (tail students) disciplines (getAboveAverageStudentsHelper scoresByCourse  student avr : res)
    where
      student = head students
      studentCourse = getStudentCourse student
      curStudentScores = [ss| ss<-studentScores, getScoreFn ss == getStudentFn student]
      scoresByCourse = getScoresByCourse curStudentScores disciplines studentCourse
      avr = getResults studentScores disciplines


getAboveAverageStudentsHelper :: [StudentScore] -> Student -> [(DisciplineName, DisciplineID, Score)] -> Maybe Student
getAboveAverageStudentsHelper [] student _ = Just student
getAboveAverageStudentsHelper studentScores student avr
  | avrScore == - 1 || (getScore score > avrScore)  = getAboveAverageStudentsHelper (tail studentScores) student avr
  | otherwise = Nothing
  where
    score = head studentScores
    id = getSCDisciplineId score
    avrScore = fromMaybe (-1) (getAvrScoreByDiscId avr id)

getAvrScoreByDiscId :: [(DisciplineName, DisciplineID, Score)] -> DisciplineID -> Maybe Score
getAvrScoreByDiscId [] _ = Nothing
getAvrScoreByDiscId ((_,id1,score):t) id = if id1 == id then Just score else getAvrScoreByDiscId t id

getScoresByCourse :: [StudentScore] -> [Discipline] -> Course -> [StudentScore]
getScoresByCourse studentScores disciplines course = helper studentScores []
  where
    helper [] res = res
    helper stSc res
      | discYear == (-1) = helper (tail stSc) res
      | discYear == course = helper (tail stSc) (score : res)
      | otherwise = helper (tail stSc) res
      where
        score = head stSc
        scoreID = getSCDisciplineId score
        discipline = getDisciplineById disciplines scoreID
        discYear = getDisciplineYear (fromMaybe ("", "", -1) discipline)

getDisciplineById :: [Discipline] -> DisciplineID -> Maybe Discipline
getDisciplineById [] _ = Nothing
getDisciplineById (h : t) x = if getDisciplineID h == x then Just h else getDisciplineById t x

--Testing
a :: [StudentScore]
a = [("46", 6, "1"), ("45", 3, "1"), ("46", 6, "2"), ("44", 4, "1"), ("45", 2, "2"), ("44", 3, "2"), ("44", 5, "3"), ("46", 6, "4"),("45",3,"4"), ("44", 4, "5"), ("45", 5, "5")]

b :: [Student]
b = [("Petko", "Kamenov", "46", 3), ("Alex", "Turner", "45", 3), ("John", "Lennon", "44", 3)]

c :: DisciplineID
c = "5"

d :: [Discipline]
d = [("Haskell", "1", 3), ("Python", "2", 3), ("C++", "3", 3), ("C#", "4", 3), ("Java", "5", 3)]

f :: [StudentScore]
f = [("46", 6, "1"), ("44", 5, "1"), ("43", 2, "1")]

main :: IO ()
main = do
  putStr $ show $ getNamesByDisciplineID a b c
  putStr "\n"
  putStr $ show $ getResults a d
  putStr "\n"
  putStr $ show $ getResults f d
  putStr "\n"
