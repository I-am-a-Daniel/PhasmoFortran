program phasmo
	implicit none
	integer :: evidenceList(7)
	integer :: evidences(4)
	integer :: i

!	Szellemek listája
	character(len=16) :: ghosts(24)
	character(len=16) :: possible_ghosts(24)
	character(len=16) :: ghosts_EMF(10)
	character(len=16) :: ghosts_UV(10)
	character(len=16) :: ghosts_Writing(10)
	character(len=16) :: ghosts_Freezing(11)
	character(len=16) :: ghosts_Dots(10)
	character(len=16) :: ghosts_Orb(10)
	character(len=16) :: ghosts_Box(11)

!	Evidence lista
	logical :: HasEMF = .false.
	logical :: HasUV = .false.
	logical :: HasWriting = .false.
	logical :: HasFreezing = .false.
	logical :: HasDots = .false.
	logical :: HasOrb = .false.
	logical :: HasBox = .false.

!	NINCS Evidence lista
	logical :: NoEMF = .false.
	logical :: NoUV = .false.
	logical :: NoWriting = .false.
	logical :: NoFreezing = .false.
	logical :: NoDots = .false.
	logical :: NoOrb = .false.
	logical :: NoBox = .false.
	
!	Szellem lista ha újra kéne inicializálni vmit
! - Ez most így lesz megoldva - lehetne őket deklarálni egyben is, de fortranban olyankor EGYFORMA karakterszám kell minden elemnek 
!	Majd baszakodunk a dinamikus allokációval később, de egyelőre lényeg h működjön.

	ghosts(1) = 'Spirit'
	ghosts(2) = 'Wraith'
	ghosts(3) = 'Phantom'
	ghosts(4) = 'Poltergeist'
	ghosts(5) = 'Banshee'
	ghosts(6) = 'Jinn'
	ghosts(7) = 'Mare'
	ghosts(8) = 'Revenant'
	ghosts(9) = 'Shade'
	ghosts(10) = 'Demon'
	ghosts(11) = 'Yurei'
	ghosts(12) = 'Oni'
	ghosts(13) = 'Yokai'
	ghosts(14) = 'Hantu'
	ghosts(15) = 'Goryo'
	ghosts(16) = 'Myling'
	ghosts(17) = 'Onryo'
	ghosts(18) = 'Twins'
	ghosts(19) = 'Raiju'
	ghosts(20) = 'Obake'
	ghosts(21) = 'Mimic'
	ghosts(22) = 'Moroi'
	ghosts(23) = 'Deogen'
	ghosts(24) = 'Thaye'
	ghosts_EMF(1) = 'Goryo'
	ghosts_EMF(2) = 'Jinn'
	ghosts_EMF(3) = 'Myling'
	ghosts_EMF(4) = 'Obake'
	ghosts_EMF(5) = 'Oni'
	ghosts_EMF(6) = 'Raiju'
	ghosts_EMF(7) = 'Shade'
	ghosts_EMF(8) = 'Spirit'
	ghosts_EMF(9) = 'Twins'
	ghosts_EMF(10) = 'Wraith'
	ghosts_UV(1) = 'Banshee'
	ghosts_UV(2) = 'Demon'
	ghosts_UV(3) = 'Goryo'
	ghosts_UV(4) = 'Hantu'
	ghosts_UV(5) = 'Jinn'
	ghosts_UV(6) = 'Myling'
	ghosts_UV(7) = 'Obake'
	ghosts_UV(8) = 'Phantom'
	ghosts_UV(9) = 'Poltergeist'
	ghosts_UV(10) = 'Mimic'
	ghosts_Writing(1) = 'Demon'
	ghosts_Writing(2) = 'Deogen'
	ghosts_Writing(3) = 'Mare'
	ghosts_Writing(4) = 'Moroi'
	ghosts_Writing(5) = 'Myling'
	ghosts_Writing(6) = 'Poltergeist'
	ghosts_Writing(7) = 'Revenant'
	ghosts_Writing(8) = 'Shade'
	ghosts_Writing(9) = 'Spirit'
	ghosts_Writing(10) = 'Thaye'
	ghosts_Freezing(1) = 'Demon'
	ghosts_Freezing(2) = 'Hantu'
	ghosts_Freezing(3) = 'Jinn'
	ghosts_Freezing(4) = 'Moroi'
	ghosts_Freezing(5) = 'Oni'
	ghosts_Freezing(6) = 'Onryo'
	ghosts_Freezing(7) = 'Revenant'
	ghosts_Freezing(8) = 'Shade'
	ghosts_Freezing(9) = 'Mimic'
	ghosts_Freezing(10) = 'Twins'
	ghosts_Freezing(11) = 'Yurei'
	ghosts_Dots(1) = 'Banshee'
	ghosts_Dots(2) = 'Deogen'
	ghosts_Dots(3) = 'Goryo'
	ghosts_Dots(4) = 'Oni'
	ghosts_Dots(5) = 'Phantom'
	ghosts_Dots(6) = 'Raiju'
	ghosts_Dots(7) = 'Thaye'
	ghosts_Dots(8) = 'Wraith'
	ghosts_Dots(9) = 'Yokai'
	ghosts_Dots(10) = 'Yurei'
	ghosts_Orb(1) = 'Banshee'
	ghosts_Orb(2) = 'Hantu'
	ghosts_Orb(3) = 'Mare'
	ghosts_Orb(4) = 'Obake'
	ghosts_Orb(5) = 'Onryo'
	ghosts_Orb(6) = 'Raiju'
	ghosts_Orb(7) = 'Revenant'
	ghosts_Orb(8) = 'Thaye'
	ghosts_Orb(9) = 'Yokai'
	ghosts_Orb(10) = 'Yurei'
	ghosts_Box(1) = 'Deogen'
	ghosts_Box(2) = 'Mare'
	ghosts_Box(3) = 'Moroi'
	ghosts_Box(4) = 'Onryo'
	ghosts_Box(5) = 'Phantom'
	ghosts_Box(6) = 'Poltergeist'
	ghosts_Box(7) = 'Spirit'
	ghosts_Box(8) = 'Mimic'
	ghosts_Box(9) = 'Twins'
	ghosts_Box(10) = 'Wraith'
	ghosts_Box(11) = 'Yokai'

!	Init
	do i = 1, 24
		!print *, ghosts(i)
		possible_ghosts(i) = ghosts(i)
	end do

!--------------------------------------------------------------Main
	!call AddEMF()			!Debug
	!call AddUV()			!Debug
	call AddWriting()		!Debug
	call AddBox()
	call AddFreezing()

	do i = 1, size(possible_ghosts)						!Kivihetjük majd subroutine-ba, ez csak a kiírás
		if (possible_ghosts(i) .ne. '') then
			print *, possible_ghosts(i)
		end if
	end do



!--------------------------------------------------------------
contains
	subroutine EmptyListForInit()			!Kell ez?
		do i = 1, 24
			possible_ghosts(i) = ''
		end do
	end subroutine EmptyListForInit

	subroutine AddEMF()
		integer :: EMFi, EMFj
		logical :: EMFvalid
		HasEMF = .true.
		do EMFi = 1, size(possible_ghosts)
			EMFvalid = .false.
			do EMFj = 1, size(ghosts_EMF)
				if (possible_ghosts(EMFi) == ghosts_EMF(EMFj)) then
					EMFvalid = .true.
				end if
			end do
			if (EMFvalid .eqv. .false.) then
				possible_ghosts(EMFi) = ''
			end if
		end do
	end subroutine AddEMF

	subroutine AddUV()
		integer :: UVi, UVj
		logical :: UVvalid
		HasUV = .true.
		do UVi = 1, size(possible_ghosts)
			UVvalid = .false.
			do UVj = 1, size(ghosts_UV)
				if(possible_ghosts(UVi) == ghosts_UV(UVj)) then
					UVvalid = .true.
				end if
			end do
			if (UVvalid .eqv. .false.) then
				possible_ghosts(UVi) = ''
			end if
		end do
	end subroutine AddUV

	subroutine AddWriting()
		integer :: Wi, Wj
		logical :: Wvalid
		HasWriting = .true.
		do Wi = 1, size(possible_ghosts)
			Wvalid = .false.
			do Wj = 1, size(ghosts_Writing)
				if(possible_ghosts(Wi) == ghosts_Writing(Wj)) then
					Wvalid = .true.
				end if
			end do
			if (Wvalid .eqv. .false.) then
				possible_ghosts(Wi) = ''
			end if
		end do
	end subroutine AddWriting

	subroutine AddFreezing()
		integer :: Freezi, Freezj
		logical :: Freezvalid
		HasFreezing = .true.
		do Freezi = 1, size(possible_ghosts)
			Freezvalid = .false.
			do Freezj = 1, size(ghosts_Freezing)
				if(possible_ghosts(Freezi) == ghosts_Freezing(Freezj)) then
					Freezvalid = .true.
				end if
			end do
			if (Freezvalid .eqv. .false.) then
				possible_ghosts(Freezi) = ''
			end if
		end do
	end subroutine AddFreezing

	subroutine AddDots()
		integer :: Dotsi, Dotsj
		logical :: Dotsvalid
		HasDots = .true.
		do Dotsi = 1, size(possible_ghosts)
			Dotsvalid = .false.
			do Dotsj = 1, size(ghosts_Dots)
				if(possible_ghosts(Dotsi) == ghosts_Dots(Dotsj)) then
					Dotsvalid = .true.
				end if
			end do
			if (Dotsvalid .eqv. .false.) then
				possible_ghosts(Dotsi) = ''
			end if
		end do
	end subroutine AddDots

	subroutine AddOrb()											!TODO: Handle Mimic
		integer :: Orbi, Orbj
		logical :: Orbvalid
		HasOrb = .true.
		do Orbi = 1, size(possible_ghosts)
			Orbvalid = .false.
			do Orbj = 1, size(ghosts_Orb)
				if(possible_ghosts(Orbi) == ghosts_Orb(Orbj)) then
					Orbvalid = .true.
				end if
			end do
			if (Orbvalid .eqv. .false.) then
				possible_ghosts(Orbi) = ''
			end if
		end do
	end subroutine AddOrb

	subroutine AddBox()
		integer :: Boxi, Boxj
		logical :: Boxvalid
		HasBox = .true.
		do Boxi = 1, size(possible_ghosts)
			Boxvalid = .false.
			do Boxj = 1, size(ghosts_Box)
				if(possible_ghosts(Boxi) == ghosts_Box(Boxj)) then
					Boxvalid = .true.
				end if
			end do
			if (Boxvalid .eqv. .false.) then
				possible_ghosts(Boxi) = ''
			end if
		end do
	end subroutine AddBox

end program phasmo



! Szóval az a terv, hogy ha valamit deletel, akkor az egész listát újra végiglépkedi. 
! Ehhez külön eltároljuk pl IsEMF, IsUV stb változókba az evidenceket, és ez alapján mehet az új init.
! Elég ocsortány megoldás, de hát ezt tudja ez a nyelv