#include "Answer.h"

#ifdef GCC
#include <sys/resource.h>
#else
#include <windows.h>
#endif

int main(int argc, char** argv)
{
#ifdef GCC
    setpriority(PRIO_PROCESS, 0, 19);
#else
    SetPriorityClass(GetCurrentProcess(), IDLE_PRIORITY_CLASS);
#endif
    printf("%lld", answer());
    return 0;
}

