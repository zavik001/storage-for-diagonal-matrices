      INTEGER FUNCTION check(n, m)
      IMPLICIT NONE 
      INTEGER n
      INTEGER m
      check = 0
      IF((n.ge.2982).or.(n.lt.4)) THEN
         check = 1
      END IF
      IF((m.ge.n-4).or.(m.lt.0)) THEN
         check = 1
      END IF
      RETURN
      END FUNCTION check

      PROGRAM MAIN
      IMPLICIT NONE
      REAL matrix(9, 2982)
      REAL vector(2982)
      REAL result(2982)
      INTEGER startIndexDiag(9)
      INTEGER n
      INTEGER m
      INTEGER type
      INTEGER check
      
      call inputT(type)

      IF (type.eq.2) THEN 
         call readInputTxt(n, m)
         IF (check(n, m).eq.1) THEN
            WRITE(*, *) 'incorrect data: n, m'
            GO TO 100
         END IF
         call initialMixingDiogonals(startIndexDiag, m)
         call readVectorTxt(vector, n)
         call readMatrixTxt(matrix, n, m)
         call multiplication(matrix, vector, result, startIndexDiag, n)
         call writeResultTxt(result, n)
      ELSE 
         call readInputBin(n, m)
         IF (check(n, m).eq.1) THEN
            WRITE(*, *) 'incorrect data: n, m'
            GO TO 100
         END IF
         call initialMixingDiogonals(startIndexDiag, m)
         call readVectorBin(vector, n)
         call readMatrixBin(matrix, n, m)
         call multiplication(matrix, vector, result, startIndexDiag, n)
         call writeResultBin(result, n)
      END IF

      GO TO 200

100   WRITE(*, *) 'ERROR'

200   END

      SUBROUTINE inputT(type)
      IMPLICIT NONE
      INTEGER type
      WRITE(*, *) 'open binary files or txt: 1- binary 2- txt'
      READ(*, *) type
      IF ((type.ge.3).or.(type.lt.1)) THEN
         WRITE(*, *) 'incorrect data: type'
         type = -1
      END IF
      END

      SUBROUTINE initialMixingDiogonals(startIndexDiag, m)
      IMPLICIT NONE
      INTEGER startIndexDiag(9)
      INTEGER m
      INTEGER i
      INTEGER j
      j = 1
      DO i = -4, 4
         IF(i.eq.0 .or. i.eq.-1 .or. i.eq.1) THEN 
            startIndexDiag(j) = i
         ELSE IF (i .lt.-1) THEN
            startIndexDiag(j) = i - m
         ELSE 
            startIndexDiag(j) = i + m
         END IF
         j = j + 1
      END DO
      END

      SUBROUTINE readInputTxt(n, m)
      IMPLICIT NONE 
      INTEGER n
      INTEGER m
      open(1, file='txtFiles/input.txt', action='READ')
      READ(1, *) n, m
      close(1)
      END

      SUBROUTINE readVectorTxt(vector, n)
      IMPLICIT NONE
      REAL vector(2982)
      INTEGER n
      INTEGER i
      open(2, file='txtFiles/vector.txt', action='READ')
      DO i = 1, n
         READ(2, *) vector(i)
      END DO
      close(2)
      END

      SUBROUTINE readMatrixTxt(matrix, n, m)
      IMPLICIT NONE
      REAL matrix(9, 2982)
      INTEGER offsets(9)
      INTEGER n
      INTEGER m
      INTEGER i, j
      open(3, file='txtFiles/matrix.txt', action='READ')
      offsets = (/ -m-4, -m-3, -m-2, -1, 0, -1, -m-2, -m-3, -m-4 /)
      DO j = 1, 9
         READ(3, *) (matrix(j, i), i = 1, n + offsets(j))
      END DO
      close(3)
      END

      SUBROUTINE writeResultTxt(result, n)
      IMPLICIT NONE
      REAL result(2982)
      INTEGER n
      INTEGER i
      open(2, file='txtFiles/resultMultip.txt', action='WRITE')
      DO i = 1, n
         WRITE(2, '(F20.10)') result(i)
      END DO
      close(2)
      END

      SUBROUTINE readInputBin(n, m)
      IMPLICIT NONE 
      INTEGER n
      INTEGER m
      open(5, file='binaryFiles/input.dat', action='READ',
     *access='DIRECT', recl=8)
      READ(5, rec=1) n
      READ(5, rec=2) m
      close(5)
      END

      SUBROUTINE readVectorBin(vector, n)
      IMPLICIT NONE
      REAL vector(2982)
      INTEGER n
      INTEGER i
      open(6, file='binaryFiles/vector.dat', action='READ', 
     *access='DIRECT', recl=8)
      DO i = 1, n
         READ(6, rec=i) vector(i)
      END DO
      close(6)
      END

      SUBROUTINE readMatrixBin(matrix, n, m)
      IMPLICIT NONE
      REAL matrix(9, 2982)
      INTEGER offsets(9)
      INTEGER n
      INTEGER m
      INTEGER i, j
      open(7, file='binaryFiles/matrix.dat', action='READ', 
     *access='DIRECT', recl=8)
      offsets = (/ -m-4, -m-3, -m-2, -1, 0, -1, -m-2, -m-3, -m-4 /)
      DO j = 1, 9
         DO i = 1,  n + offsets(j)
            READ(7, rec=(j-1)*(n+1) + i) matrix(j, i)
         END DO
      END DO
      close(7)
      END

      SUBROUTINE writeResultBin(result, n)
      IMPLICIT NONE
      REAL result(2982)
      INTEGER n
      INTEGER i
      open(6, file='binaryFiles/resultMultip.dat', action='WRITE', 
     *access='DIRECT', recl=8)
      DO i = 1, n
         WRITE(6, rec=i) result(i)
      END DO
      close(6)
      END

      SUBROUTINE multiplication(matrix, vector, result, startIndexDiag, 
     *n)
      IMPLICIT NONE 
      REAL matrix(9, 2982)
      REAL vector(2982)
      REAL result(2982)
      INTEGER startIndexDiag(9)
      INTEGER n
      INTEGER i, j, d, start
      
      DO i = 1, n
         DO d = 1, 9
            start = startIndexDiag(d)
            j = i + start
            IF (j.ge.1 .and. j.LE.n) THEN
               IF (start.ge.1) THEN
                  result(i) = result(i) + matrix(d, i) * vector(j)
               ELSE 
                  result(i) = result(i) + matrix(d, j) * vector(j)
               END if
            END IF
         END DO
      END DO

      END
