      PROGRAM CONVERSION
      IMPLICIT NONE
      REAL matrix(9, 2982)
      REAL vector(2982)
      REAL result(2982) 
      INTEGER n
      INTEGER m
      INTEGER type

      call inputT(type)

      IF (type.eq.1) THEN 
         call readInputTxt(n, m)
         call readVectorTxt(vector, n)
         call readMatrixTxt(matrix, n, m)
         call readResultTxt(result, n)
         call writeInputBin(n, m)
         call writeVectorBin(vector, n)
         call writeResultBin(result, n)
         call writeMatrixBin(matrix, n, m)
      ELSE IF (type.eq.2) THEN
         call readInputBin(n, m)
         call readVectorBin(vector, n)
         call readMatrixBin(matrix, n, m)
         call readResultBin(result, n)
         call writeInputTxt(n, m)
         call writeVectorTxt(vector, n)
         call writeResultTxt(result, n)
         call writeMatrixTxt(matrix, n, m)
      ELSE 
         GO TO 100
      END IF
      GO TO 200

100   WRITE(*, *) 'ERROR'
200   END

      SUBROUTINE inputT(type)
      IMPLICIT NONE
      INTEGER type
      WRITE(*, *) '1: txtFiles/*.txt -> binaryFiles/*.dat'
      WRITE(*, *) '2: binaryFiles/*.dat -> txtFiles/*.txt'
      READ(*, *) type
      IF ((type.ge.3).or.(type.lt.1)) THEN
         WRITE(*, *) 'incorrect data: type'
         type = -1
      END IF
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
         DO i = 1, n + offsets(j)
            READ(7, rec=(j-1)*(n+1) + i) matrix(j, i)
         END DO
      END DO
      close(7)
      END

      SUBROUTINE readResultTxt(result, n)
      IMPLICIT NONE
      REAL result(2982)
      INTEGER n
      INTEGER i
      open(2, file='txtFiles/resultMultip.txt', action='READ')
      DO i = 1, n
         READ(2, *) result(i)
      END DO
      close(2)
      END

      SUBROUTINE readResultBin(result, n)
      IMPLICIT NONE
      REAL result(2982)
      INTEGER n
      INTEGER i
      open(6, file='binaryFiles/resultMultip.dat', action='READ', 
     *access='DIRECT', recl=8)
      DO i = 1, n
         READ(6, rec=i) result(i)
      END DO
      close(6)
      END

      SUBROUTINE writeInputBin(n, m)
      IMPLICIT NONE 
      INTEGER n
      INTEGER m
      open(5, file='binaryFiles/input.dat', action='WRITE', 
     *access='DIRECT', recl=8)
      WRITE(5, rec=1) n
      WRITE(5, rec=2) m
      close(5)
      END

      SUBROUTINE writeInputTxt(n, m)
      IMPLICIT NONE 
      INTEGER n
      INTEGER m
      open(1, file='txtFiles/input.txt', action='WRITE')
      write(1, '(I20, 1X)', advance='no') n
      write(1, '(I20)') m
      close(1)
      END

      SUBROUTINE writeVectorBin(vector, n)
      IMPLICIT NONE
      REAL vector(2982)
      INTEGER n
      INTEGER i
      open(6, file='binaryFiles/vector.dat', action='WRITE', 
     *access='DIRECT', recl=8)
      DO i = 1, n
         WRITE(6, rec=i) vector(i)
      END DO
      close(6)
      END

      SUBROUTINE writeMatrixBin(matrix, n, m)
      IMPLICIT NONE
      REAL matrix(9, 2982)
      INTEGER offsets(9)
      INTEGER n
      INTEGER m
      INTEGER i, j
      open(7, file='binaryFiles/matrix.dat', action='WRITE', 
     *access='DIRECT', recl=8)
      offsets = (/ -m-4, -m-3, -m-2, -1, 0, -1, -m-2, -m-3, -m-4 /)
      DO j = 1, 9
         DO i = 1, n + offsets(j)
            WRITE(7, rec=(j-1)*(n+1) + i) matrix(j, i)
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

      SUBROUTINE writeMatrixTxt(matrix, n, m)
      IMPLICIT NONE
      REAL matrix(9, 2982)
      INTEGER offsets(9)
      INTEGER n
      INTEGER m
      INTEGER i, j
      open(2, file='txtFiles/matrix.txt', action='WRITE')
      offsets = (/ -m-4, -m-3, -m-2, -1, 0, -1, -m-2, -m-3, -m-4 /)
      DO j = 1, 9
         DO i = 1, n + offsets(j)
            write(2, '(F20.10, 1X)', advance='no') matrix(j, i)
         END DO
         write(2, *)
      END DO
      close(2)
      END

      SUBROUTINE writeVectorTxt(vector, n)
      IMPLICIT NONE
      REAL vector(2982)
      INTEGER n
      INTEGER i
      open(2, file='txtFiles/vector.txt', action='WRITE')
      DO i = 1, n
         WRITE(2, '(F20.10)') vector(i)
      END DO
      close(2)
      END

