      module subs
      contains
        subroutine execute(label, ins, init_regs)
          implicit none
          character(len=*) :: label
          character(len=3), dimension(:,:), allocatable :: ins
          integer*4, dimension(4) :: regs, init_regs
          integer :: pc, jmp, rx, ry
          integer*4 :: vx, vy
          pc = 1; regs = init_regs
          do
            if (pc < 0 .or. pc > size(ins,1)) exit
            jmp = 1
            rx = index('abcd',trim(ins(pc,2)))
            ry = index('abcd',trim(ins(pc,3)))
            if (rx == 0) read (ins(pc,2),*) vx
            if (rx /= 0) vx = regs(rx)
            if (ry == 0) read (ins(pc,3),*) vy
            if (ry /= 0) vy = regs(ry)
            select case (ins(pc,1))
              case ('inc')
                regs(rx) = vx + 1
              case ('dec')
                regs(rx) = vx - 1
              case ('cpy')
                regs(ry) = vx
              case ('jnz')
                if (vx /= 0) jmp = vy
            end select
            pc = pc + jmp
          end do
          print *,label,':',regs(1)
          return
        end subroutine execute
      end module subs

      program solution
        use subs
        implicit none
        integer :: eof, sz, pc
        character(len=3), dimension(:,:), allocatable :: ins
        ! count instructions in input file
        sz = 0
        open(1,file='./input')
        do
          read(1, *, iostat=eof)
          if (eof /= 0) exit
          sz = sz + 1
        end do
        ! read instructions from input file
        rewind(1)
        allocate(ins(sz,3))
        do pc=1, sz
          read(1, '(*(A))') ins(pc,1:3)
          ins(pc,2) = adjustl(ins(pc,2))
          ins(pc,3) = adjustl(ins(pc,3))
        end do
        close(1)
        call execute('part1',ins,(/0,0,0,0/))
        call execute('part2',ins,(/0,0,1,0/))
      end program solution
