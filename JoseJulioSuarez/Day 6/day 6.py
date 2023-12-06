input = 'input.txt'
records = []
bigRecord = ()
with open(input) as file:
    times = file.readline().strip().replace('        ', ' ').replace('     ',' ').split(' ')[1:]
    time = ''
    for element in times:
        time+=element
    distances = file.readline().strip().split('   ')[1:]
    distance = ''
    for element in distances:
        distance+=element
    bigRecord = (time, distance)
    records = list(zip(times, distances))

def recordBeaterCalc(entry):
    chances = 0
    time = int(entry[0])
    goal = int(entry[1])
    for charge in range(0,time+1):
        distance = charge*(time-charge)
        if distance > goal:
            chances += 1
    return chances   

part1 = 1
for record in records:
    part1 = part1*recordBeaterCalc(record)

part2 = recordBeaterCalc(bigRecord)

print(part1, part2)