#include<iostream>
#include <iomanip>
#include<fstream>
#include<cstdio>
#include<bitset>
#include<string.h>
using namespace std;
bool isnum(char c)
{
    if((c<='9'&&c>='0')||(c<='F'&&c>='A')) return true;
    else return false;
}
unsigned int convertnum(char c)
{
    unsigned int tmp;
    if((c<='9'&&c>='0')) tmp=c-'0';
    else tmp=c-'A'+10;
    return tmp;
}
void read(unsigned int& PC)
{
    unsigned int x=0;
    char ch=getchar();
    while(isnum(ch)){
        ch = (ch<'A')?(ch-'0'):(ch-'A'+10);
        x=(x<<4)+ch;ch=getchar();
    }
    PC= x;
}
unsigned int read()//读入32位指令
{
    unsigned int ins=0;
    char s[8];
    s[0]=getchar();

    while(!isnum(s[0])) {
        if(s[0]==EOF) return 0;
        if(s[0]=='@') return 2;
        s[0]=getchar();
    }
    s[1]=getchar(); getchar();
    ins=(convertnum(s[0])<<4)|convertnum(s[1]);
    for(int i=2;i<8;i+=2)
    {
        s[i]=getchar();
        s[i+1]=getchar(); getchar();
        ins|=convertnum(s[i])<<(4*(i+1));
        ins|=convertnum(s[i+1])<<(4*i);
    }
    return ins;
}

const char* name[] = {"LUI","AUIPC","JAL","JALR","BEQ","BNE","BLT","BGE","BLTU","BGEU","LB","LH","LW","LBU","LHU","SB","SH","SW","ADDI","SLTI","SLTIU","XORI","ORI","ANDI","SLLI","SRLI","SRAI","ADD","SUB","SLL","SLT","SLTU","XOR","SRL","SRA","OR","AND"};
const char* REGISTER_name[] = {"zero","ra","sp","qp","tp","t0","t1","t2","s0","s1","a0","a1","a2","a3","a4","a5","a6","a7","s2","s3","s4","s5","s6","s7","s8","s9","s10","s11","t3","t4","t5","t6"};
enum instructtype{
    LUI=1,
    AUIPC,
    JAL,
    JALR,
    BEQ,
    BNE,
    BLT,
    BGE,
    BLTU,
    BGEU,
    LB,
    LH,
    LW,
    LBU,
    LHU,
    SB,
    SH,
    SW,
    ADDI,
    SLTI,
    SLTIU,
    XORI,
    ORI,
    ANDI,
    SLLI,
    SRLI,
    SRAI,
    ADD,
    SUB,
    SLL,
    SLT,
    SLTU,
    XOR,
    SRL,
    SRA,
    OR,
    AND
};

int REGISTER[32];
unsigned int PC;//当前地址
int MEMORY[0x1000000];
bool stall=false;//数据冒险暂停流水
bool end_=false;
bool cancel=false;//跳转错误改为空操作
int predictright;//预测正确数
int cnt;//总循环数
int cancelnum;//取消的操作数


struct instruction
{
    unsigned int instructionnum;
    int imm=0;
    unsigned short rs1;//寄存器
    unsigned short rs2;//寄存器
    unsigned short rd;//目标寄存器
    instructtype instructiontype;
    char instructgenre;
    instruction(){};
    void ConvertType(unsigned int num)
    {
        instructionnum=num;
        imm=0;
        unsigned int func3 = (num>>12)%8;//cout<<func3<<endl;
        unsigned int func7 = (num>>30);
        int a=0;
        for(int i=6;i>=0;i--)
        {
            int tmp=(num>>i)%2;
            a=10*a+tmp;
        }
        //cout<<a<<endl;
        switch(a)
        {
            case 11:
                if(func3==0)instructiontype=LB;
                else if(func3==1)instructiontype=LH;
                else if(func3==2)instructiontype=LW;
                else if(func3==4)instructiontype=LBU;
                else instructiontype=LHU;
                break;
            case 100011:
                if(func3==0)instructiontype=SB;
                else if(func3==1)instructiontype=SH;
                else if(func3==2)instructiontype=SW;
                break;
            case 110011:
                switch(func3){
                    case 0:
                    if(func7==0) instructiontype=ADD;
                    else instructiontype=SUB; break;
                    case 1:
                    instructiontype=SLL;  break;
                    case 2:
                    instructiontype=SLT; break;
                    case 3:
                    instructiontype=SLTU; break;
                    case 4:
                    instructiontype=XOR; break;
                    case 5:
                    if(func7==1) instructiontype = SRA;
                            else instructiontype = SRL; break;
                    case 6:
                    instructiontype=OR;break;
                    case 7:
                    instructiontype=AND;break;
                } break;
                
            case 10011:
                switch(func3){
                    case 0:
                    instructiontype=ADDI; break;
                    case 1:
                    instructiontype=SLLI; break;
                    case 2:
                    instructiontype=SLTI; break;
                    case 3:
                    instructiontype=SLTIU; break;
                    case 4:
                    instructiontype=XORI; break;
                    case 5:
                    if(func7==1) instructiontype = SRAI;
                            else instructiontype = SRLI; break;
                    case 6:
                    instructiontype=ORI; break;
                    case 7:
                    instructiontype=ANDI;break;
                }
                break;
                
            case 110111:
                instructiontype=LUI; break;
            case 10111:
                instructiontype=AUIPC;break;
            case 1100011:
                switch(func3){
                    case 0:
                    instructiontype=BEQ;break;
                    case 1:
                    instructiontype=BNE;break;
                    case 4:
                    instructiontype=BLT;break;
                    case 5:
                    instructiontype=BGE;break;
                    case 6:
                    instructiontype=BLTU;break;
                    case 7:
                    instructiontype=BGEU;break;
                };
                break;
            case 1101111:
                instructiontype=JAL;break;
            case 1100111:
                instructiontype=JALR;break;
        }
        if(instructiontype==LUI||instructiontype==AUIPC) instructgenre='U';
        else if(instructiontype==JAL) instructgenre='J';
        else if(instructiontype>4&&instructiontype<11) instructgenre='B';
        else if(instructiontype>15&&instructiontype<19) instructgenre='S';
        else if(instructiontype>27) instructgenre='R';
        else instructgenre='I';
    }
    void InstructionDecode()
    {
        switch (instructgenre)
        {
        case 'U':
            imm=instructionnum&0xFFFFF000UL;
            rd=(instructionnum>>7)%32;
            break;
        case 'J':
            rd = (instructionnum>>7)%32;
            imm|=(instructionnum&0x80000000UL)>>31-20;
            imm|=(instructionnum&0x7FE00000UL)>>30-10;
            imm|=(instructionnum&0x100000UL)>>20-11;
            imm|=(instructionnum&0xFF000UL);
            for(int i=21;i<=31;i++) imm|=(instructionnum&0x80000000UL)>>31-i;
            break;
        case 'B':
            rs1 = (instructionnum>>15)%32;
            rs2 = (instructionnum>>20)%32;
            imm|=(instructionnum&0x80000000UL)>>31-12;
            imm|=(instructionnum&0x7E000000UL)>>30-10;
            imm|=(instructionnum&0xF00UL)>>11-4;
            imm|=(instructionnum&0x80UL)<<11-7;
            for(int i=12;i<=31;i++) imm|=(instructionnum&0x80000000UL)>>31-i;
            break;
        case 'I':
            rs1 = (instructionnum>>15)%32;
            rd = (instructionnum>>7)%32;
            imm |= ((instructionnum & 0xFFF00000UL)>>31-11);
            for(int i=12;i<=31;i++) imm|=(instructionnum&0x80000000UL)>>31-i;
            break;
        case 'S':
            rs1 = (instructionnum>>15)%32;
            rs2 = (instructionnum>>20)%32;
            imm|= ((instructionnum & 0xFE000000UL)>>31-11);
            imm|=(instructionnum&0xF80UL)>>11-4;
            for(int i=12;i<=31;i++) imm|=(instructionnum&0x80000000UL)>>31-i;
            break;
        case 'R':
            rs1 = (instructionnum>>15)%32;
            rs2 = (instructionnum>>20)%32;
            rd  = (instructionnum>>7)%32;
            break;
        default:
            break;
        }
    }
    void clear(){
        imm=0;
        rs1=0;
        rs2=0;
        rd=0;
    }
} i;

class RISCV_Pipeline
{
    private:

        struct {
            unsigned int IR;//指令
            unsigned int NPC;//地址
        } IF_ID;
        struct{
            bitset<2> BranchHistoryTable[5000];
            int BranchTargetBuffer[5000];
        } BranchCache;
        struct{
            int A;
            int B;
            int NPC;
            unsigned int IR;
            instructtype instructiontype;
            char instructiongenre;
            unsigned short rd;
            unsigned short rs2;
            int Imm;
        } ID_EX;
        struct{
            int ALUOutput;
            instructtype instructiontype;
            char instructiongenre;
            unsigned short rd;
            unsigned short rs2;
            int B;
            bool cond;//是否为分支
        } EX_MEM;
        struct{
            instructtype instructiontype;
            char instructiongenre;
            unsigned short rd;
            unsigned short rs2;
            int ALUOutput;
            int LMD;
        } MEM_WB;
    void InstructionFetch()
    {
        IF_ID.IR=MEMORY[PC/4];
        if(BranchCache.BranchHistoryTable[PC&0xfffU][1]==1){
            IF_ID.NPC=PC+4;
            PC=BranchCache.BranchTargetBuffer[PC&0xfffU];
        }
        else{
            IF_ID.NPC=PC+4;
            PC+=4;
        }

    }
    void InstructionDecode()
    {
        if(IF_ID.IR==0x0ff00513) end_=true;
        stall=false;
        ID_EX.IR=IF_ID.IR;
        i.ConvertType(IF_ID.IR);
        i.InstructionDecode();
        ID_EX.instructiongenre=i.instructgenre;
        ID_EX.instructiontype=i.instructiontype;
        ID_EX.rd=i.rd;
        ID_EX.rs2=i.rs2;
        ID_EX.Imm=i.imm;
        ID_EX.A=REGISTER[i.rs1];
        ID_EX.B=REGISTER[i.rs2];
        ID_EX.NPC=IF_ID.NPC;
        if(MEM_WB.instructiontype<=4||MEM_WB.instructiontype>=19)
        {
            if(i.rs1!=0&&i.rs1==MEM_WB.rd) ID_EX.A=MEM_WB.ALUOutput;
            if(i.rs2!=0&&i.rs2==MEM_WB.rd) ID_EX.B=MEM_WB.ALUOutput;
        }
        if(MEM_WB.instructiontype>10&&MEM_WB.instructiontype<16)
        {
            if(i.rs1!=0&&i.rs1==MEM_WB.rd) ID_EX.A=MEM_WB.LMD;
            if(i.rs2!=0&&i.rs2==MEM_WB.rd) ID_EX.B=MEM_WB.LMD;
        }
        if(EX_MEM.instructiontype<=4||EX_MEM.instructiontype>=19)
        {
            if(i.rs1!=0&&i.rs1==EX_MEM.rd) ID_EX.A=EX_MEM.ALUOutput;
            if(i.rs2!=0&&i.rs2==EX_MEM.rd) ID_EX.B=EX_MEM.ALUOutput;
        }
        if(EX_MEM.instructiontype>10&&EX_MEM.instructiontype<16)
        {
            if((i.rs1!=0&&i.rs1==EX_MEM.rd)||(i.rs2!=0&&i.rs2==EX_MEM.rd)) {
                ID_EX.A=0;
                ID_EX.B=0;
                ID_EX.rs2=0;
                ID_EX.instructiongenre='R';
                ID_EX.instructiontype=ADD;
                ID_EX.rd=0;
                stall=true;
            }
        }
        i.clear();
    }
    //修改BHT
    void PredictionRight()
    {
        int PCLow=(ID_EX.NPC-4)&0xfffU;//低八位
        BranchCache.BranchTargetBuffer[PCLow]=PC;
        if(BranchCache.BranchHistoryTable[PCLow]==bitset<2>(3)){    
            BranchCache.BranchHistoryTable[PCLow]=bitset<2>(3);
        }else if(BranchCache.BranchHistoryTable[PCLow]==bitset<2>(2)){
            BranchCache.BranchHistoryTable[PCLow]=bitset<2>(3);
        }else{
            BranchCache.BranchHistoryTable[PCLow]=bitset<2>(0);
        }
        predictright++;
    }
    void PredictionWrong()
    {
        int PCLow=(ID_EX.NPC-4)&0xfffU;//低八位
        if(BranchCache.BranchHistoryTable[PCLow]==bitset<2>(0)){    
            BranchCache.BranchHistoryTable[PCLow]=bitset<2>(1);
        }else if(BranchCache.BranchHistoryTable[PCLow]==bitset<2>(2)){
            BranchCache.BranchHistoryTable[PCLow]=bitset<2>(1);
        }else{
            BranchCache.BranchHistoryTable[PCLow]=bitset<2>(2);
        }
    }
    void jump(int PClow,int pos1){
        if(BranchCache.BranchHistoryTable[PClow][1]==1&&pos1==BranchCache.BranchTargetBuffer[PClow]){
            PredictionRight();
        }else{
            if(BranchCache.BranchHistoryTable[PClow][1]==1) PredictionRight();
            else PredictionWrong();
            PC=pos1;
            IF_ID.IR=0x33U;
            BranchCache.BranchTargetBuffer[PClow]=PC;
            cancel=true;
            cancelnum++;
        }

    }
        void nonjump(int PClow,int pos1){
        if(BranchCache.BranchHistoryTable[PClow][1]==0){
            PredictionRight();
        }else{
                PredictionWrong();
                IF_ID.IR=0x33U;
                PC=ID_EX.NPC;
                cancel=true;
                cancelnum++;
        }
    }
    void Execute()
    {
        EX_MEM.instructiongenre=ID_EX.instructiongenre;
        EX_MEM.instructiontype=ID_EX.instructiontype;
        EX_MEM.rd=ID_EX.rd;
        EX_MEM.rs2=ID_EX.rs2;
        int PClow=(ID_EX.NPC-4)&0xfffU;
        int pos1=ID_EX.NPC+ID_EX.Imm-4;//可能的目标地址
        switch(ID_EX.instructiontype)
        {
            case 1:{//LUI
                EX_MEM.ALUOutput=ID_EX.Imm; break;
            }
            case 2:{
                EX_MEM.ALUOutput=ID_EX.Imm+PC; break;
            }
            case 3:{//JAL
                if(BranchCache.BranchHistoryTable[PClow][1]==1){
                    PredictionRight();
                    if(pos1==BranchCache.BranchTargetBuffer[PClow]){
                            EX_MEM.ALUOutput=ID_EX.NPC;
                    }else{
                    EX_MEM.ALUOutput=ID_EX.NPC;
                    PC=pos1; 
                    IF_ID.IR=0x33U;
                    BranchCache.BranchTargetBuffer[PClow]=PC;
                    cancel=true;
                    // cancelnum++;
                    }
                }else{
                    PredictionWrong();
                    EX_MEM.ALUOutput=ID_EX.NPC;
                    PC=pos1; 
                    IF_ID.IR=0x33U;
                    BranchCache.BranchTargetBuffer[PClow]=PC;
                    cancel=true;
                    // cancelnum++;
                } 
                break;
            }
            case 4:{//JALR
                if(BranchCache.BranchHistoryTable[PClow][1]==1&&((ID_EX.Imm+ID_EX.A)&(unsigned int)-2)==BranchCache.BranchTargetBuffer[PClow]){
                    PredictionRight();
                    EX_MEM.ALUOutput=ID_EX.NPC;
                }else {
                    if(BranchCache.BranchHistoryTable[PClow][1]==1) PredictionRight();
                    else PredictionWrong();
                    EX_MEM.ALUOutput=ID_EX.NPC;
                    PC=(ID_EX.Imm+ID_EX.A)&(unsigned int)-2;
                    IF_ID.IR=0x33U; 
                    BranchCache.BranchTargetBuffer[PClow]=PC;
                    cancel=true;
                    // cancelnum++;
                }
                break;
            }
            case 5://BEQ
                if(ID_EX.A==ID_EX.B){
                    jump(PClow,pos1);
                } else {
                    nonjump(PClow,pos1);
                }
                break;
            case 6://BNE
                if(ID_EX.A!=ID_EX.B){
                    jump(PClow,pos1);
                }  else{
                    nonjump(PClow,pos1);
                }
                break;
            case 7://BLT
                if(ID_EX.A<ID_EX.B){
                    jump(PClow,pos1);
                } else{
                    nonjump(PClow,pos1);
                }
                break;
            case 8://BGE
                if(ID_EX.A>=ID_EX.B){
                    jump(PClow,pos1);
                }else{
                    nonjump(PClow,pos1);
                } 
                break;
            case 9://BLTU
                if((unsigned int)ID_EX.A<(unsigned int)ID_EX.B){
                    jump(PClow,pos1);
                }else{
                   nonjump(PClow,pos1);
                } 
                break;
            case 10://BGEU
                if((unsigned int)ID_EX.A>=(unsigned int)ID_EX.B){
                    jump(PClow,pos1);
                }else{
                    nonjump(PClow,pos1);
                } 
                break;
            case 11:{//LB
                EX_MEM.ALUOutput=ID_EX.A+ID_EX.Imm; break;
            }
            case 12:{//LH
                EX_MEM.ALUOutput=ID_EX.A+ID_EX.Imm; break;
            }
            case 13:{//LW
                EX_MEM.ALUOutput=ID_EX.A+ID_EX.Imm; break;
            }
            case 14:{//LBU
                EX_MEM.ALUOutput=ID_EX.A+ID_EX.Imm; break;
            }
            case 15:{//LHU
                EX_MEM.ALUOutput=ID_EX.A+ID_EX.Imm; break;
            }
            case 16:{//SB
                EX_MEM.ALUOutput=ID_EX.A+ID_EX.Imm;
                EX_MEM.B=ID_EX.B; break;
            }
            case 17:{//SH
                EX_MEM.ALUOutput=ID_EX.A+ID_EX.Imm;
                EX_MEM.B=ID_EX.B; break;
            }
            case 18:{//SW
                EX_MEM.ALUOutput=ID_EX.A+ID_EX.Imm;
                EX_MEM.B=ID_EX.B; break;
            }
            case 19:{//ADDI
                EX_MEM.ALUOutput=ID_EX.Imm+ID_EX.A; break;
            }
            case 20:{//SLTI
                int temp;
                if(ID_EX.A<ID_EX.Imm) temp=1;
                else temp=0;
                EX_MEM.ALUOutput=temp; break;
            }
            case 21:{//SLTIU
                unsigned int imm1=ID_EX.Imm;
                int temp;
                temp=ID_EX.A<imm1;
                EX_MEM.ALUOutput=temp;
                break;
            }
            case 22:{//XORI
                EX_MEM.ALUOutput=ID_EX.A^ID_EX.Imm; break;
            }
            case 23:{//ORI
                EX_MEM.ALUOutput=ID_EX.A|ID_EX.Imm; break;
            }
            case 24:{//ANDI
                EX_MEM.ALUOutput=ID_EX.A&ID_EX.Imm; break;
            }
            case 25:{//SLLI
                EX_MEM.ALUOutput=(unsigned int)ID_EX.A<<(ID_EX.Imm&31UL); break;
            }
            case 26:{//SRLI
                EX_MEM.ALUOutput=(unsigned int)ID_EX.A>>(ID_EX.Imm&31UL); break;
            }
            case 27:{//SRAI
                EX_MEM.ALUOutput=ID_EX.A>>(ID_EX.Imm&31UL); break;
            }
            case 28://ADD
                EX_MEM.ALUOutput=ID_EX.A+ID_EX.B; break;
            case 29://SUB
                EX_MEM.ALUOutput=ID_EX.A-ID_EX.B; break;
            case 30://SLL
                EX_MEM.ALUOutput=(unsigned int)ID_EX.A<<(ID_EX.B&31UL); break;
            case 31://SLT
                EX_MEM.ALUOutput=ID_EX.A<ID_EX.B; break;
            case 32:{//SLTU
                int temp;
                if((unsigned int)ID_EX.A<(unsigned int)ID_EX.B) temp=1;
                else temp=0;
                EX_MEM.ALUOutput=temp; break;
            }
            case 33://XOR
                EX_MEM.ALUOutput=ID_EX.A^ID_EX.B; break;
            case 34://SRL
                EX_MEM.ALUOutput=(unsigned int)ID_EX.A>>(ID_EX.B&31UL); break;
            case 35://SRA
                EX_MEM.ALUOutput=ID_EX.A>>(ID_EX.B&31UL); break;
            case 36://OR
                EX_MEM.ALUOutput=ID_EX.A|ID_EX.B; break;
            case 37://AND
                EX_MEM.ALUOutput=ID_EX.A&ID_EX.B; break;
            }
            if(ID_EX.instructiontype<=2||ID_EX.instructiontype>=11){
                if(!cancel)
                {
                    if(BranchCache.BranchHistoryTable[PClow][1]==1){
                    PredictionWrong();
                    IF_ID.IR=0x33U;
                    PC=ID_EX.NPC;
                    }else PredictionRight();
                }else cancel=false;
            }
    }
    void MemoryAccess()
    {
        MEM_WB.instructiongenre=EX_MEM.instructiongenre;
        MEM_WB.instructiontype=EX_MEM.instructiontype;
        MEM_WB.rd=EX_MEM.rd;
        MEM_WB.rs2=EX_MEM.rs2;
        if(MEM_WB.instructiontype<=4||MEM_WB.instructiontype>=19)
        {
            MEM_WB.ALUOutput=EX_MEM.ALUOutput;
        }else if(MEM_WB.instructiontype>=11)
        {
            switch(MEM_WB.instructiontype){
                case 11:{//LB
                    char c;
                    memcpy(&c,(char*)MEMORY+EX_MEM.ALUOutput,sizeof(char));
                    MEM_WB.LMD=(int )c; break;
                }
                case 12:{//LH
                    short c;
                    memcpy(&c,(char*)MEMORY+EX_MEM.ALUOutput,sizeof(short));
                    MEM_WB.LMD=(int )c; break;
                }
                case 13:{//LW
                    int c;
                    memcpy(&c,(char*)MEMORY+EX_MEM.ALUOutput,sizeof(int));
                    MEM_WB.LMD=c; break;
                }
                case 14:{//LBU
                    unsigned char c;
                    memcpy(&c,(char*)MEMORY+EX_MEM.ALUOutput,sizeof(char));
                    MEM_WB.LMD=(int )c; break;
                }
                case 15:{//LHU
                    unsigned short c;
                    memcpy(&c,(char*)MEMORY+EX_MEM.ALUOutput,sizeof(short));
                    MEM_WB.LMD=(int )c; break;
                }
                case 16:{//SB
                    char c=(char) EX_MEM.B;
                    memcpy((char *)MEMORY+EX_MEM.ALUOutput,&c,sizeof(char)); break;
                }
                case 17:{//SH
                    short c=(short) EX_MEM.B;
                    memcpy((char *)MEMORY+EX_MEM.ALUOutput,&c,sizeof(short)); break;
                }
                case 18:{//SW
                    memcpy((char *)MEMORY+EX_MEM.ALUOutput,&EX_MEM.B,sizeof(int)); break;
                }
            }
        }
    }
    void WriteBack()
    {
        if(MEM_WB.instructiontype<=4||MEM_WB.instructiontype>=19)
        {
            REGISTER[MEM_WB.rd]=MEM_WB.ALUOutput;
            if(MEM_WB.rd==0) REGISTER[MEM_WB.rd]=0;
        }else if(MEM_WB.instructiontype<16&&MEM_WB.instructiontype>10){
            REGISTER[MEM_WB.rd]=MEM_WB.LMD;
            if(MEM_WB.rd==0) REGISTER[MEM_WB.rd]=0;
        }
        
    }
    public:
        void run()
        {
            while(true)
            {
                
                if(cnt>=4) WriteBack();
                if(cnt>=3)MemoryAccess();
                if(cnt>=2)Execute();
                if(cnt>=1)InstructionDecode();
                if(end_) break;
                if(stall) continue;
                //cout<<"=========="<<endl;
                //if(cnt>2){cout<<name[ID_EX.instructiontype-1]<<endl;printf("%x\n",ID_EX.IR);}
                //printf("%x\n",PC);
                //cout<<"a"<<endl;
                InstructionFetch();
                //cout<<(((unsigned int)REGISTER[10])&(255u))<<endl;
                if(ID_EX.IR==0x0ff00513) break;
                cnt++;
            }

        }
};
int main()
{
    //freopen(".\\riscv-testcases\\testcases\\pi.data","r",stdin);
    while (true){
            unsigned int ins=read();
            //printf("%x\n",ins);
            if(ins==0) break;
            if(ins==2) {read(PC);
            //printf("move to 0x%x\n",PC);
            continue;
            };
            MEMORY[PC/4]=ins;
            PC+=4;
    }

    PC=0;
    RISCV_Pipeline pipeline;
    pipeline.run();
    int ans=(((unsigned int)REGISTER[10])&(255u));
    cout<<ans<<endl;
    //cout<<(float)predictright/(cnt-cancelnum);
}