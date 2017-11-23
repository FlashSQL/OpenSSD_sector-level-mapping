# Buffer Management and Flow Control
[reference](http://www.openssd-project.org/wiki/Barefoot_Buffer_Management)
GNU Free Documentation License, Version 1.3 


- SATA data is buffered in DRAM(cannot use SRAM for SATA data buffering). 
- 2 spaces needed: for read buffering & write buffering
- controller should know the position of each.
- 3rd buffer space needed (for flash copy-back)


## 1. Read Buffer
 an array of multiple read buffers that form a circular buffer system.

### 1-1. read buffer configuration.
- **SATA_BUF_PAGE_SIZE**: The size of each read buffer. (applied to write buffers and flash copy buffer)
- **SATA_RBUF_BASE**: start address of the read buffer space.
> The address should be an integer multiple of **SATA_BUF_PAGE_SIZE** value.
- **SATA_RBUF_SIZE**: The number of buffers in the read buffer

### 1-2. SATA read operation and read buffer.
- set option as FO_B_STAT_R(flash read operation)
> SETREG(**FCP_OPTION**, * | **FO_B_SATA_R**)
- the address pointed to by FCP_DMA_ADDR should be a read buffer.
> SETREG(**FCP_DMA_ADDR**, RD_BUF_PTR(buffer id...))

- The controller identifies a buffer by its ID. 
**Read_buffer_ID = (buffer_addr - SATA_RBUF_BASE) / SATA_BUF_PAGE_SIZE**

- For the flow control of SATA read operation, there are three buffer pointers. 

### 1-3. Three buffer pointers for the flow control of SATA read operation.
#### 1-3-1. FTL read buffer pointer
- Every time the firmware issue a flash read command with **FO_B_SATA_R**, associated read buffer ID should increase by one.
- if the buffer ID of the last command was 7, next command should be for 8.
- firmware should initialize the global variable to zero upon the system initialization and ensure it is always less than SATA_RBUF_SIZE
#### 1-3-2. SATA_RBUF_PTR
- This regiser holds the buffer ID from which the current SATA read transfer is being done.
- While SATA read transfer is not taking place, it points to the buffer ID of the most recent transfer.
- cannot be equal to or larger than SATA_RBUF_SIZE. (circular nature.)
#### 1-3-3. BM_READ_LIMIT
- maintained by the buffer manager hardware and is used by SATA hardware.
- SATA read transfer can make progress only while BM_READ_LIMIT is ahead of SATA_RBUF_PTR.
- every page transfer finish > SATA hardware checks whether **SATA_RBUF_PTR == BM_READ_LIMIT**.

- When the firmware issues a flash read command with FO_B_SATA_ R, it should make sure the associated **read buffer ID != current value of BM_READ_LIMIT or SATA_RBUF_PTR**
- defining the number of read buffers > the number of banks, BM_READ_LIMIT don't need.



## 2. Write Buffer
 an array of multiple write buffers that form a circular buffer system. 

### 2-1. write buffer configuration
- **SATA_BUF_PAGE_SIZE**: the size of each write buffer.
- **SATA_WBUF_BASE**": the start address of the write buffer space.
> the address should be an integer multiple of **SATA_BUF_PAGE_SIZE** value
- **SATA_WBUF_SIZE**: the number of buffers in the write buffer space.

### 2-2. SATA write operation and read buffer
- set option as FO_B_SATA_W (flash write operation)
> SETREG(**FCP_OPTION**, *| **FO_B_SATA_W**)
- The address pointed to by FCP_DMA_ADDR should be a write buffer
> SETREG(**FCP_DMA_ADDR**, WR_BUF_PTR(buffer id ...))

- The controller idnetifies a buffer by its ID.
**Write_buffer_ID = (buffer_addr - SATA_WBUF_BASE) / SATA_BUF_PAGE_SIZE**

### 2-3. Three buffer pointers for the flow control of SATA write operation
#### 2-3-1. FTL write buffer pointer
- Every time the firmware issues a flash write command with **FO_B_SATA_W**, associated write buffer ID should be increase by one.
- The firmware should initialize the global variable to zero upon the system initialization phase and ensure it is always less than SATA_WBUF_SIZE.
#### 2-3-2. SATA_WBUF_PTR
- holds the buffer ID to which the current SATA write transfer is being done.
- cannot be equal to or larger than SATA_WBUF_SIZE
#### 2-3-3. BM_WRITE_LIMIT
- maintained by the buffer manager hardware and used by SATA hardware.
- write progress can only be made while BM_WRITE_LIMIT is ahead of SATA_WBUF_PTR.
- every page transfer finish > SATA hardware checks whether **SATA_WBUF_PTR == BM_READ_LIMIT**

- firmware is not responsible for the flow control of write stream.
- it is allowed for the firmware to issue a command with FO_B_SATA_W, even before the data is ready.
- The number of write buffers should be large enough to ensure that no two banks accept write commands with the sam buffer ID.



## 3. Flash Copy Buffer 
 an array of multiple buffers for flash copy-back operation. 

### 3-1 without copy back steps
- A page is read and the data is transferred from flash to DRAM. 
> **FC_COL_ROW_READ_OUT** is used for this step.
- The data is transferred back to flash and programmed to a new page. 
> **FC_COL_ROW_IN_PROC** is used for this step.

### 3-2 using copy back steps
- A page is read, by read-for-copy-back command and the data is transferred from flash to DRAM. 
- One or more corrected sectors are transferred back to flash. 
- The data is programmed to a new page.
> **FC_COPYBACK* covers all three steps.

### 3-3 copy buffer configuration
- each bank exclusively owns its flash copy buffer
- **SATA_BUF_PAGE_SIZE**: the size of each buffer.
- **FCONF_CPBMABASE**: register which let the hardware know its start address.



## 4. Misalignment
 there are three types of buffers: **read buffer**, **write buffer**, **flash copy buffer**
 the size of each buffer: **SATA_BUF_PAGE_SIZE**
 - as the basic unit of data transfer to/from buffers is not a page but sector(512bytes) a transfer may not align to buffer boundary.

 - e.g. SATA read transfer
```
buffer_DRAM_ADDR = SATA_RBUF_BASE + SATA_RBUF_PTR * SATA_BUF_PAGE_SIZE
sect_offset = SATA_SECT_OFFSET % (SATA_BUF_PAGE_SIZE / 512)
transfer_start_addr = (buffer_DRAM_ADDR) + (sect_offset) * 512
```
 - e.g. SATA write transfer
```
buffer_DRAM_ADDR = SATA_WBUF_BASE + SATA_WBUF_PTR * SATA_BUF_PAGE_SIZE
sect_offset = SATA_SECT_OFFSET % (SATA_BUF_PAGE_SIZE / 512)
transfer_start_addr = (buffer_DRAM_ADDR) + (sect_offset) * 



