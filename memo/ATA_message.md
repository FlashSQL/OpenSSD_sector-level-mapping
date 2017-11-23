# ATA messages via SCSI layer
[reference](http://www.tldp.org/HOWTO/SCSI-Generic-HOWTO/index.html)

## 1. SCSI subsystem
 - **libata**
> the name of a set of drivers and some glue code that **allows the SCSI layer to talk to ATA devices**. 
>
> support **HDIO_DRIVE_CMD** and **HDIO_DRIVE_TASK** for backwards compatibility (for hdparm, hdtemp, smartmontools etc.)
>
> but! does not support **HDIO_DRIVE_TASKFILE**: it suggested that people use SG_IO instead.

`HDIO_DRIVE_TASK: can issue commands that have LBA commands, cannot read or write data`
`HDIO_DRIVE_CMD: can issue commands that read blocks of data, cannot send commands with LBA`
`HDIO_DRIVE_TASKFILE: can issue **pretty much anyting** but is more complicated to use` 

 - **SG_IO**
> allows the same features as the IDE TASKFILE, but sends commands through the SCSI subsystem
> 
> SCSI & ATA commands are nothing alike libata maintains translation code. 


## 2. sg_io_hdr_t structure
 - **main control structure**
	[i] notation indicates an input value
	[o] indicates a value that is output.
	[i->o] indicates a value that is conveyed from input to output and is not unsed by the driver
	[*i] pointer that is used for readng for reading from user mem to driver
	[*o] pointer used for writing
	[*io] pointer used for either reading or writing
```
typedef struct sg_io_hdr
{
    int interface_id;           /* [i] 'S' (required) */
    int dxfer_direction;        /* [i] */
    unsigned char cmd_len;      /* [i] */
    unsigned char mx_sb_len;    /* [i] */
    unsigned short iovec_count; /* [i] */
    unsigned int dxfer_len;     /* [i] */
    void * dxferp;              /* [i], [*io] */
    unsigned char * cmdp;       /* [i], [*i]  */
    unsigned char * sbp;        /* [i], [*o]  */
    unsigned int timeout;       /* [i] unit: millisecs */
    unsigned int flags;         /* [i] */
    int pack_id;                /* [i->o] */
    void * usr_ptr;             /* [i->o] */
    unsigned char status;       /* [o] */
    unsigned char masked_status;/* [o] */
    unsigned char msg_status;   /* [o] */
    unsigned char sb_len_wr;    /* [o] */
    unsigned short host_status; /* [o] */
    unsigned short driver_status;/* [o] */
    int resid;                  /* [o] */
    unsigned int duration;      /* [o] */
    unsigned int info;          /* [o] */
} sg_io_hdr_t;  /* 64 bytes long (on i386) */

```

### 2-1. interface_id 
- int
- must be set to 'S'(capital ess). If not ENOSYS error placed in error
- allow interface variants in the futer that dentify themselves with a different value.

### 2-2. dxfer_direction
- int
- required to be one of the following
**SG_DXFER_NONE**		: e.g. a SCSI Test unit ready command
> 	used when there is no data transfer associated with a command
**SG_DXFER_TO_DEV**		: e.g. a SCSI WRITE command
> 	used when data is being moved from user memory towards the device
**SG_DXFER_FROM_DEV**		: e.g. a SCSI READ command
>	used when data is being moved from the device towards user memory
**SG_DXFER_TO_FROM_DEV**
>	relevant to indirect IO. (otherwise it is treated like SG_DXFER_FROM_DEV)
>	data is moved from the user space to the kernel buffers, then performed and most likely a READ-like command transfers data from the device into the kernel buffers.
**SG_DXFER_UNKNOWN**
> for rare situations where the data direction is not known. 

- dxfer_direction must have one of the five indicated values, cannot be uninitialized or zero.

### 2-3. cmd_len
- unsigned char
- length in bytes of the SCSI command that 'cmdp' pointes to.
- EMSGSIZE error number is produced if the value is less thatn 6 or greater than 16.

### 2-4. mx_sb_len
- unsigned char
- maximum size that can be written back to the 'sbp'pointer
- actual number written out is given by 'sb_len_wr'. 
- in all case 'sb_len_wr' <= 'mx_sb_len'

### 2-5. iovec_count
- the number of scatter gather elements in an array pointed to by 'dxferp'.
- if zero scatter gather is not being used. 
- if value is greater than zero, each element of the array is assumed to be of the form
```
typedef struct sg_iovec
{
	void iov_base; /*starting address*/
	size_t iov_len; /*length in bytes*/
}
```
- When the SG_FLASH_DIRECT_IO or SG_FLAG_MMAP_IO are set in flags iovec_count should be zero
