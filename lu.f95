program lu_decomposition

	implicit none

	integer, parameter :: n = 3 !size of the matrix

	real(kind=16), dimension(n,n) :: L, U, A
	A = reshape((/real::5,1,3,3,2,0,2,0,4/), (/n,n/))

	call decompose(L, U, A)

	print *, "matrix A:"
	call print_matrix(A)
	print *, "matrix L:"
	call print_matrix(L)
	print *, "matrix U:"
	call print_matrix(U)

	contains

	subroutine decompose(L, U, A)

		implicit none
		real(kind=16), dimension(n,n) :: L, U, A
		integer i,j,k

		do i=1, n
			l(i,i) = 1
		enddo

		do i=1, n
			do j=i, n
				u(i,j) = a(i,j)
				do k=1, i-1
					u(i,j) = u(i,j) - l(i,k) * u(k,j)
				enddo
			enddo
			do j=i+1, n
				l(j,i) = a(j,i)
				do k=1, i-1
					l(j,i) = l(j,i) - l(j,k) * u(k,i)
				enddo
				l(j,i) = l(j,i) / u(i,i)
			enddo
		enddo

	end subroutine decompose

	subroutine print_matrix(matrix)

		implicit none
		real(kind=16), dimension(n,n) :: matrix
		integer i,j, n, m
		m = 3
		n = 3
		do i=1, m
		    do j=1, n
		        write (*,"(dc,f12.3)", advance="no") matrix(i,j)
		    enddo
		    print *
		enddo
		print *

	end subroutine print_matrix

end program lu_decomposition

